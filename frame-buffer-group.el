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
;; around frames. Buffers are placed in groups, and a frame can be
;; bound to a group. The buffers in a group are visible (shown in
;; switch-to-buffer) in this frame, but are not visible in another
;; frame that are not bound to the group. The relationship between
;; buffer, group, and frame is,
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
;; The default group is the unnamed group, "*", which every newly created
;; frame is bound to. The command frame-buffer-group-bind-group binds
;; the frame to a group, a named one or the unnamed one. If the group
;; does not exist, it is created. If the current group is the unnamed
;; group, the buffers that are visible when the command is called are
;; placed in the group. After a frame is bound to a group, all buffers
;; in the group are visible in the frame.
;;
;; A typical workflow is,
;; - create a new frame (this frame is bound to the unnamed group)
;; - bind the frame to a new group foo
;; - work in the frame
;; - buffers opened in the frame are visible
;; - need another frame to work on the same thing
;; - create a new frame
;; - bind the frame to the group foo
;; - to work on something new, create a new frame
;;
;; The buffer local variable frame-buffer-group-groups has all groups
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
  (let ((bufs (Buffer-menu-marked-buffers)))
		(setq bufs (append bufs (frame-parameter nil 'buffer-list)))
		(set-frame-parameter nil 'buffer-list (seq-uniq bufs)))
	(fbg/ensure-buffers-visible))

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
	 (list (completing-read "Bind to group: " fbg/all-groups)))
	(and (fbg/empty-string-p group) (setq group "*"))
	(unless (equal (fbg/group) group)
		(fbg/bind-to nil group)))

(defun frame-buffer-group-remove-current-buffer ()
	"Move current buffer from this group to the unnamed group."
  (interactive)
	(let* ((group (fbg/group))
				 (frames (fbg/frames-of-group group))
				 (buf (current-buffer))
				 (pred (lambda (w) (eq buf (window-buffer w)))))
		(and (equal group "*") (error "cannot remove from unnamed group"))
		(fbg/remove-buffer-from-group group)
		(fbg/add-buffer-to-group "*")	
		(dolist (frame frames)
			(fbg/switch-to-scratch frame pred))
		(dolist (frame frames)
			(set-frame-parameter frame 'buffer-list (delete buf (frame-parameter frame 'buffer-list))))) )

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
	;; A group may be bounded with multiple frames. A buffer that is
	;; newly opened in one frame should be visible in all these frames.
	;; We add this buffer to all these frames when one of them gains
	;; focus or the frame where the buffer is opened is deleted.
	(add-hook 'focus-in-hook 'fbg/ensure-buffers-visible)
	(add-hook 'delete-frame-functions 'fbg/ensure-buffers-visible)
	(customize-push-and-save 'desktop-locals-to-save '(frame-buffer-group-groups)))

(defun fbg/disable ()
	;; keep frame-buffer-group-groups in desktop-locals-to-save
	(remove-hook 'delete-frame-functions 'fbg/ensure-buffers-visible)
	(remove-hook 'focus-in-hook 'fbg/ensure-buffers-visible)
	(remove-hook 'after-make-frame-functions 'fbg/after-make-frame)
	(dolist (func fbg/builtin-function-whitelist)
		(when (fboundp func)
			(advice-remove func 'fbg/apply-with-builtin-buffer-list)))
	(advice-remove 'buffer-list 'fbg/buffer-list))

(defun fbg/buffer-managed-p (b)
	(let ((name (buffer-name b)))
		(and (not (string-prefix-p "*" name)) (not (string-prefix-p " " name)))))

(defun fbg/restore-groups ()
	(dolist (f (frame-list))
		(unless (frame-parameter f 'frame-buffer-group-group)
			(fbg/set-frame-group f "*")))
	(let ((n 0)
				names)
		(dolist (b (fbg/builtin-buffer-list)) 
			(with-current-buffer b
				(setq names (append names frame-buffer-group-groups))
				;; place buffer in unnamed
				(when (and (fbg/buffer-managed-p b) (not frame-buffer-group-groups))
					(setq n (1+ n))
					(fbg/add-buffer-to-group "*"))))
		(setq fbg/all-groups (or (seq-uniq names) '("*")))
		(message "restored groups:%s; added %d buffers to unnamed" names n))
	(fbg/ensure-buffers-visible))

(defun fbg/ensure-buffers-visible (&optional frame)
	;; ensure invariants:
	;; - a buffer in this frame must be in the group this frame is bound to
	;; - all frames that are bound to the same group must show all buffers in the group
	;;
	;; this frame ->
	;; group ->
	;; all frames bound to this group ->
	;; all buffers (if a buffer has no group, set group) except "*" and " " ->
	;; ensure buffers are in all frames
	(let* ((group (fbg/group frame))
				 (frames (fbg/frames-of-group group))
				 (buf-in-group-p (lambda (b)
													 (with-current-buffer b
														 (member group frame-buffer-group-groups))))
				 all-bufs)
		;; collect all buffers in frames
		(dolist (f frames)
			(setq all-bufs (append all-bufs
														 (seq-filter 'fbg/buffer-managed-p (frame-parameter f 'buffer-list)))))
		;; collect all buffers in group
		(setq all-bufs (append all-bufs
													 (seq-filter buf-in-group-p (fbg/builtin-buffer-list))))
		(setq all-bufs (seq-uniq all-bufs))
		;; ensure buffers are in group 
		(dolist (b all-bufs)
			(with-current-buffer b
				(fbg/add-buffer-to-group group)))
		;; ensure buffers in all frames
		(dolist (f frames)
			(fbg/add-buffers-to-frame f all-bufs))))

(defun fbg/add-buffers-to-frame (frame bufs)
	(let* ((old (frame-parameter frame 'buffer-list))
				 (combined (seq-uniq (append old bufs))))
		(unless (eq (length old) (length combined))
			(set-frame-parameter frame 'buffer-list combined))))

(setq fbg/stick-buffers
			(list
			 (get-buffer "*scratch*")
			 (get-buffer "*Messages*")))

(defun fbg/after-make-frame (frame)
	;; switch to *scratch* to hide the current buffer
	;; bind frame to unname group
	(fbg/set-frame-group frame "*")
	(fbg/switch-to-scratch frame)
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

(defun fbg/bind-to (frame group)
	(let ((old (fbg/group frame)))
		(and (fbg/empty-string-p old) (error "empty old group"))
		(unless (equal "*" old)
			(fbg/switch-to-scratch frame))
		(set-frame-parameter frame 'buffer-list nil)
		(fbg/set-frame-group frame group)
		(unless (member group fbg/all-groups)
			(setq fbg/all-groups (cons group fbg/all-groups))))
	(fbg/ensure-buffers-visible))

(defun fbg/frames-of-group (group)
	(or (member group fbg/all-groups) (error "unknown group:%s" group))
	(seq-filter
	 (lambda (f) (equal group (fbg/group f)))
	 (frame-list)))

(defun fbg/empty-string-p (s)
	(or (not s) (eq 0 (length s))))

(defun fbg/add-buffer-to-group (group)
	(let ((lst frame-buffer-group-groups))
		(unless (member group lst)
			(setq frame-buffer-group-groups (cons group lst)))))

(defun fbg/remove-buffer-from-group (group)
	(or (member group frame-buffer-group-groups) (error "buffer not in group:%s" group))
	(setq frame-buffer-group-groups (delete group frame-buffer-group-groups)))

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
