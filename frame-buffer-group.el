;;; frame-buffer-group.el --- Every frame is a buffer group    -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Bin Zhang

;; Author: Bin Zhang <zncoder@gmail.com>
;; Keywords: lisp
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; FrameBufferGroup provides a straightforward way to organize buffers
;; around frames. Buffers that are opened in a frame are visible only
;; in this frame.  These buffers form a group. We say the frame is
;; bound to the group. When a frame is bound to a group, all buffers
;; in the group are visible in the frame. As we open new buffers in
;; the frame, the buffers are added to the group and are visible in
;; the frame.  Buffers in the group are not visible in frames that are
;; not bound to the group.  We can bind multiple frames to the same
;; group, and make the buffers in the group visible in all these
;; frames.  The relationship between buffer, group, and frame is,
;; - a buffer can be in N groups
;; - a group can have N buffers
;; - a frame is bound to 1 group
;; - a group can be bound to N frames
;; For example,
;; - B1 -> (G1, G2)
;; - B2 -> (G1)
;; - B3 -> (G1)
;; - F1 -> G1
;; - F2 -> G2
;; - F3 -> G1
;; In this example, buffer B1, B2 and B3 are visible in frame F1 and F3, and B1 is visible in F2.
;;
;; The default group is the unnamed group, "*", which every newly
;; created frame is bound to. If we don't create a new group, the
;; behavior is no different from the normal behavior. All buffers are
;; visible in all frames. We bind the frame to a new group or an
;; existing group with the command frame-buffer-group-bind-to. This
;; command binds the selected frame to either a named one or the
;; unnamed one. If the group does not exist, it is created. To add a
;; buffer that is in another group to this group, the one the selected
;; frame is bound to, we use the command
;; frame-buffer-group-builtin-switch-to-buffer, which makes all
;; buffers available to switch to. Once a buffer is visible in the
;; frame, it is added to the group the frame is bound to.
;;
;; A typical workflow is,
;; - create a new frame (this frame is bound to the unnamed group)
;; - bind the frame to a new group foo
;; - work in the frame
;; - buffers opened in the frame are visible
;; - need another frame to work on the same thing
;; - create a new frame
;; - bind the frame to the group foo
;; - to work on something new, create a new frame, and bind it to
;;   another group.
;;
;; The buffer local variable frame-buffer-group-groups lists all groups
;; the buffer is in. It is added to desktop-locals-to-save to persist
;; the group info.

;;; Installation:

;; Add to ~/.emacs
;;
;;    (require 'frame-buffer-group)
;;    (frame-buffer-group-mode 1)

;;; Code:

(defun frame-buffer-group-mode (arg)
  "Toggle frame buffer group mode.
Positive ARG enables the mode. Zero or negative ARG disable the
mode.  When the mode is on, buffers are associated with the frame
where they are opened."
  (interactive "P")
  (let ((old fbg/enabled)
        (new (> (prefix-numeric-value arg) 0)))
    (unless (eq old new)
      (if new
          (fbg/enable)
        (fbg/disable))
      (setq fbg/enabled new))))

(defvar-local frame-buffer-group-groups nil
  "Groups this buffer is in")

(defun frame-buffer-group-add-buffers-from-buffer-menu ()
  "Add buffers marked in buffer list to this group"
  (interactive)
  (fbg/add-buffers-to-frame nil (Buffer-menu-marked-buffers))
  (fbg/fix-buffers-in-frame))

(defun frame-buffer-group-builtin-switch-to-buffer ()
  "Use builtin switch-to-buffer. All buffers are visible."
  (interactive)
  (fbg/call-interactively-with-builtin-buffer-list 'switch-to-buffer))

(defun frame-buffer-group-list-buffers ()
  "List buffers in this group."
  (interactive)
  (let ((bufs (frame-parameter nil 'buffer-list)))
    (display-buffer (list-buffers-noselect nil bufs))))

(defun frame-buffer-group-bind-to (group)
  "Bind this frame to group NAME."
  (interactive
   (let ((other-groups (remove (fbg/group) fbg/all-groups)))
     (list (completing-read "Bind to group: " other-groups nil nil (car other-groups)))))
  (and (fbg/empty-string-p group) (setq group "*"))
  (and (equal group (fbg/group)) (error "same group"))
  (or (member group fbg/all-groups)
      (setq fbg/all-groups (cons group fbg/all-groups)))
  (fbg/bind-to nil group))

(defun frame-buffer-group-remove-current-buffer ()
  "Move current buffer from this group to the unnamed group."
  (interactive)
  (let* ((group (fbg/group))
         (frames (fbg/frames-of-group group))
         (buf (current-buffer))
         (pred (lambda (w) (eq buf (window-buffer w)))))
    (and (equal group "*") (< (length frame-buffer-group-groups) 2)
         (error "cannot remove from the sole unnamed group"))
    (fbg/remove-buffer-from-group group)
    (fbg/add-buffer-to-group "*") 
    (dolist (frame frames)
      (fbg/switch-to-scratch frame pred))
    (dolist (frame frames)
      (delq buf (frame-parameter frame 'buffer-list)))))

;; (defun frame-buffer-group-merge-to (group))?

(unless (fboundp 'fbg/builtin-buffer-list)
  (fset 'fbg/builtin-buffer-list (symbol-function 'buffer-list)))

(setq fbg/enabled nil
      fbg/all-groups nil)

(defun fbg/enable ()
  (fbg/restore-groups)
  (advice-add 'buffer-list :override 'fbg/buffer-list)
  (dolist (func fbg/builtin-function-whitelist) 
    (when (fboundp func)
      (advice-add func :around 'fbg/apply-with-builtin-buffer-list)))
  (add-hook 'after-make-frame-functions 'fbg/after-make-frame)
  ;; We have 2 cases to handle for a buffer with file
  ;; 1/ buffer is restored by desktop. The groups are already set.
  ;; 2/ buffer is created during the session. No group is set.
  (add-hook 'find-file-hook 'fbg/add-file-buffer-to-group)
  ;; buffer with no file is handled in focus-in-hook and
  ;; delete-frame-functions. focus-in-hook is used because we don't
  ;; have per-frame focus-out-hook.
  (add-hook 'focus-in-hook 'fbg/fix-buffers-in-frame)
  (add-hook 'delete-frame-functions 'fbg/fix-buffers-in-frame)
  (customize-push-and-save 'desktop-locals-to-save '(frame-buffer-group-groups))
  ;; when a file buffer is restored, the frame-buffer-group-groups is
  ;; not set when find-file-hook is called
  (add-hook 'desktop-after-read-hook 'fbg/restore-groups))

(defun fbg/disable ()
  (remove-hook 'desktop-after-read-hook 'fbg-restore-groups)
  ;; keep frame-buffer-group-groups in desktop-locals-to-save
  (remove-hook 'delete-frame-functions 'fbg/fix-buffers-in-frame)
  (remove-hook 'focus-in-hook 'fbg/fix-buffers-in-frame)
  (remove-hook 'find-file-hook 'fbg/add-file-buffer-to-group)
  (remove-hook 'after-make-frame-functions 'fbg/after-make-frame)
  (dolist (func fbg/builtin-function-whitelist)
    (when (fboundp func)
      (advice-remove func 'fbg/apply-with-builtin-buffer-list)))
  (advice-remove 'buffer-list 'fbg/buffer-list))

(defun fbg/star-buffer-p (b)
  (string-prefix-p "*" (buffer-name b)))

(defun fbg/ignored-buffer-p (b)
  (string-prefix-p " " (buffer-name b)))

(defun fbg/restore-groups ()
  ;; put frames without group to *
  (dolist (f (frame-list))
    (or (frame-parameter f 'frame-buffer-group-group)
        (fbg/set-frame-group f "*")))
  ;; restore groups
  (let ((names '("*")))
    (dolist (b (fbg/builtin-buffer-list))
      (with-current-buffer b
        (setq names (append names frame-buffer-group-groups))))
    (setq fbg/all-groups (seq-uniq names)))
  (message "restore groups:%s" fbg/all-groups)
  ;; group buffers by group
  (let ((group-bufs (make-hash-table :test 'equal)) ;group -> (buf)
        star-bufs)
    (dolist (b (fbg/builtin-buffer-list))
      (cond
       ((fbg/star-buffer-p b)
        (setq star-bufs (cons b star-bufs)))
       ((fbg/ignored-buffer-p b))
       (t
        (or frame-buffer-group-groups (setq frame-buffer-group-groups '("*")))
        (dolist (g frame-buffer-group-groups)
          (puthash g (cons b (gethash g group-bufs)) group-bufs)))))
    ;; make buffers visible in the frames
    (dolist (f (frame-list))
      (let ((g (frame-parameter f 'frame-buffer-group-group)))
        (fbg/add-buffers-to-frame f (append star-bufs (gethash g group-bufs)))))))

(defun fbg/buffer-in-group-p (buf group)
  (and (not (fbg/star-buffer-p buf))
       (not (fbg/ignored-buffer-p buf))
       (with-current-buffer buf
         (member group frame-buffer-group-groups))))

(defun fbg/fix-buffers-in-group (group)
  ;; ensure buffers in group is visible in all frames that are bound to group
  (let ((bufs (seq-remove
               (lambda (b)
                 (or (fbg/star-buffer-p b)
                     (fbg/ignored-buffer-p b)
                     (with-current-buffer b
                       (not (member group frame-buffer-group-groups)))))
               (fbg/builtin-buffer-list)))
        (frames (fbg/frames-of-group group)))
    (dolist (f frames)
      (fbg/add-buffers-to-frame f bufs))))  

(defun fbg/fix-buffers-in-frame ()
  ;; ensure invariants:
  ;; - a buffer in this frame must be in the group this frame is bound to
  ;; - all frames that are bound to the same group must show all buffers in the group
  (let* ((group (fbg/group))
         (frames (fbg/frames-of-group group)))
    ;; ensure buffers that are visible in frames that are bound to the group are in this group
    (dolist (f frames)
      (dolist (b (frame-parameter f 'buffer-list))
        (or (fbg/star-buffer-p b)
            (fbg/ignored-buffer-p b)
            (with-current-buffer b
              (unless (member group frame-buffer-group-groups)
                (fbg/add-buffer-to-group group))))))
    (fbg/fix-buffers-in-group group)
    ;; ensure star buffers are in frames
    (let ((star-bufs (seq-filter 'fbg/star-buffer-p (fbg/builtin-buffer-list))))
      (dolist (f frames)
        (fbg/add-buffers-to-frame f star-bufs)))))

(defun fbg/add-buffers-to-frame (frame bufs)
  (let ((old (frame-parameter frame 'buffer-list)))
    (set-frame-parameter frame 'buffer-list (seq-uniq (append old bufs)))))

(setq fbg/stick-buffers
      (list
       (get-buffer "*scratch*")
       (get-buffer "*Messages*")))

(defun fbg/after-make-frame (frame)
  ;; switch to *scratch* to hide the current buffer
  ;; bind frame to unname group
  (fbg/set-frame-group frame "*")
  (fbg/bind-to frame "*")
  (fbg/add-buffers-to-frame frame fbg/stick-buffers))

(defun fbg/set-frame-group (frame group)
  (and (fbg/empty-string-p group) (error "empty group"))
  (set-frame-parameter frame 'frame-buffer-group-group group)
  (let ((title (frame-parameter frame 'title))
        (new-title (if (equal "*" group) nil (concat "⋅" group "⋅"))))
    (when (or (not title) (string-prefix-p "⋅" title))
      (set-frame-parameter frame 'title new-title))))

(defun fbg/group (&optional frame)
  (let ((group (frame-parameter frame 'frame-buffer-group-group)))
    (or group (error "frame has no group"))
    group)) 

(defun fbg/switch-to-scratch (frame &optional pred)
  (dolist (w (window-list frame))
    (with-selected-window w
      (when (or (not pred) (funcall pred w))
        (switch-to-buffer "*scratch*")))))

;; todo remember group window configuration in frame-parameter

(defun fbg/bind-to (frame group)
  (fbg/fix-buffers-in-frame)
  (fbg/switch-to-scratch frame)
  (set-frame-parameter frame 'buffer-list nil)
  (fbg/set-frame-group frame group)
  (fbg/fix-buffers-in-group group))

(defun fbg/frames-of-group (group)
  (or (member group fbg/all-groups) (error "unknown group:%s" group))
  (seq-filter
   (lambda (f) (equal group (fbg/group f)))
   (frame-list)))

(defun fbg/empty-string-p (s)
  (or (not s) (eq 0 (length s))))

(defun fbg/add-file-buffer-to-group ()
  (if frame-buffer-group-groups
      ;; restored from desktop
      (dolist (g frame-buffer-group-groups)
        (or (member g fbg/all-groups)
            (setq fbg/all-groups (cons g fbg/all-groups)))
        (fbg/fix-buffers-in-group g))
    ;; new file buffer
    (let ((group (fbg/group)))
      (fbg/add-buffer-to-group group)
      (fbg/fix-buffers-in-group group))))

(defun fbg/add-buffer-to-group (group)
  (let ((lst frame-buffer-group-groups))
    (or (member group lst)
        (setq frame-buffer-group-groups (cons group lst)))))

(defun fbg/remove-buffer-from-group (group)
  (or (member group frame-buffer-group-groups) (error "buffer not in group:%s" group))
  (delete group frame-buffer-group-groups))

(setq fbg/major-mode-whitelist
      '(Buffer-menu-mode))

(defun fbg/buffer-list (&optional frame)
  (if (memq major-mode fbg/major-mode-whitelist) 
      (fbg/builtin-buffer-list frame)
    (frame-parameter frame 'buffer-list)))

(defun fbg/call-interactively-with-builtin-buffer-list (cmd)
  (cl-letf (((symbol-function 'buffer-list)
             #'fbg/builtin-buffer-list))
    (call-interactively cmd)))

(defun fbg/apply-with-builtin-buffer-list (func &rest args)
  (cl-letf (((symbol-function 'buffer-list)
             #'fbg/builtin-buffer-list))
    (apply func args)))

;; use builtin buffer-list for some functions
(setq fbg/builtin-function-whitelist
      '(desktop-save))

(provide 'frame-buffer-group)
;;; frame-buffer-group.el ends here
