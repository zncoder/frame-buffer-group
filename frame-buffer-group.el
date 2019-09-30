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

;; FrameBufferGroup provides a simple way to organize buffers around
;; frames. Buffers that are opened in a frame stay with the frame and
;; are not visible in other frames (not shown in
;; switch-to-buffer). They form a buffer group. A new frame starts in
;; an anonymous group. You can set a name to the group. Named group
;; can be attached to multiple frames in case you need multiple frames
;; to work with these buffers. Named groups can also be saved in emacs
;; desktop.
;;
;; A typical workflow is,
;; - to work on something new, create a new frame
;; - open buffers in this frame and work on them
;; - set a name to make this group survive emacs restarts or when you
;;   need anothe frame to work on these buffers
;; - create a new frame (during this emacs session or after emacs
;;   restart), and set a name to the same group to attach the group to it
;;
;; A buffer can be in multiple groups. When you switch to a buffer in
;; another frame, the buffer is added to the group of the other
;; frame. You can remove the current buffer from the group. To visit a
;; buffer in another group, you can call
;; frame-buffer-group-switch-to-buffer, which shows all buffers, not
;; just the buffers in this group.

;;; Installation:

;; Add to ~/.emacs
;;
;;    (require 'frame-buffer-group)
;;    (frame-buffer-group-mode 1)
;;
;; To save buffer groups in emacs desktop, customize
;; desktop-locals-to-save to include frame-buffer-group-groups-buffer-in.

;;; Code:

(defvar frame-buffer-group-mode nil)

(defun frame-buffer-group-mode (arg)
	"Toggle focused frame mode.
Positive ARG enables the mode. Zero or negative ARG disable the
mode.  When the mode is on, buffers are associated with the frame
where they are opened."
	(interactive "P")
	(setq frame-buffer-group-mode
				(if arg (> (prefix-numeric-value arg) 0)
					(not frame-buffer-group-mode)))
	(if frame-buffer-group-mode
			(fbg/enable)
		(fbg/disable)))

(defvar-local frame-buffer-group-groups-buffer-in nil
	"List of groups this buffer is in")

(setq	fbg/all-group-names nil
			fbg/timer nil)

(unless (fboundp 'fbg/builtin-buffer-list)
	(fset 'fbg/builtin-buffer-list (symbol-function 'buffer-list)))

(defun frame-buffer-group-set-name (name)
	"Set group of this frame to NAME."
	(interactive
	 (list (completing-read "Group name (empty to remove): " fbg/all-group-names)))
	(if (eq 0 (length name))
			(fbg/remove-name)
		(let ((oldname (fbg/frame-group)))
			(unless (equal oldname name)
				(when (> (length oldname) 0)
					(fbg/remove-name))
				(fbg/add-name name)))))

(defun fbg/add-name (name)
	(let* ((frame (selected-frame))
				 (bufs (frame-parameter frame 'buffer-list))
				 (otherframes (fbg/frames-in-group name))
				 (otherbufs (frame-parameter (car otherframes) 'buffer-list)))
		(setq fbg/all-group-names (fbg/add-to-list name fbg/all-group-names))
		(fbg/set-frame-group frame name)
		(if (eq 1 (length (fbg/frames-in-group name)))
				;; some buffers may already in this group and
				;; this is the first frame in the group
				(fbg/attach-buffers-to-frame frame name)
			(fbg/merge-buffers name))))

(defun fbg/remove-name ()
	(let* ((frame (selected-frame))
				 (name (fbg/frame-group frame)))
		(when (> (length name) 0)
			(fbg/set-frame-group frame nil)			;remove
			(unless (fbg/frames-in-group name)
				;; no other frames in this group,
				;; remove buffers from the group and the group itself
				(fbg/update-groups-buffer-in frame name t)
				(setq fbg/all-group-names (delete name fbg/all-group-names))))))

;; ensure all buffers in this frame are in the group
(defun fbg/ensure-buffers-in-group (&optional frame)
	(let* ((frame (or frame (selected-frame)))
				 (name (fbg/frame-group frame)))
		(fbg/update-groups-buffer-in frame name)))

;; ensure all buffers are in the correct groups and frames.
;; for each group, get the list of frames and make sure
;; buffers of these frames are in the group.
(defun fbg/ensure-state ()
	(dolist (name fbg/all-group-names)
		(let ((frames (fbg/frames-in-group name)))
			(when frames
				(fbg/ensure-buffers-in-group (car frames))))))

(defun fbg/update-groups-buffer-in (frame name &optional remove)
	(when name
		(dolist (b (frame-parameter frame 'buffer-list))
			(with-current-buffer b
				(if remove
						(fbg/remove-buffer-from-group name)
					(fbg/add-buffer-to-group name))))))

(defun fbg/remove-buffer-from-group (name)
	(setq frame-buffer-group-groups-buffer-in
				(delete name frame-buffer-group-groups-buffer-in)))

(defun fbg/add-buffer-to-group (name)
	(setq frame-buffer-group-groups-buffer-in
				(fbg/add-to-list name frame-buffer-group-groups-buffer-in)))

;; ensure all buffers in all frames of the same group are in every frame
(defun fbg/merge-buffers (&optional name)
	(let* ((name (or name (fbg/frame-group)))
				 (frames (fbg/frames-in-group name))
				 (allbufs (make-hash-table :test 'eq)))
		(dolist (frame frames)
			(dolist (b (frame-parameter frame 'buffer-list))
				(puthash b t allbufs)
				(with-current-buffer b
					(let ((bufname (buffer-name)))
						(unless (or (string-prefix-p " " bufname) (string-prefix-p "*" bufname))
							(fbg/add-buffer-to-group name))))))
		(let ((n (hash-table-count allbufs))
					bufs)
			(maphash (lambda (b v) (setq bufs (cons b bufs))) allbufs)
			(dolist (frame frames)
				(unless (eq n (length (frame-parameter frame 'buffer-list)))
					(fbg/reset-frame-buffer-list frame bufs))))))

(defun fbg/frame-group (&optional frame)
	(when (frame-parameter frame 'frame-buffer-group-p)
		(frame-parameter frame 'title)))

(defun fbg/set-frame-group (frame name)
	(modify-frame-parameters
	 frame
	 `((title . ,name)
		 (frame-buffer-group-p . ,(and name t)))))

(defun fbg/add-to-list (elt lst)
	(if (member elt lst)
			lst
		(cons elt lst)))

(defun fbg/start-restore ()
	(if (and (boundp 'desktop-lazy-timer) desktop-lazy-timer)
			;; wait for desktop lazy load to finish
			(run-with-idle-timer 5 nil 'fbg/start-restore)
		(fbg/restore)))

(defun fbg/restore ()
	(let ((bufs (fbg/builtin-buffer-list))
				(frames (frame-list))
				(allnames (make-hash-table :test 'equal))
				names)
		;; restore groups
		(dolist (b bufs)
			(with-current-buffer b
				(dolist (name frame-buffer-group-groups-buffer-in)
					(puthash name t allnames))))
		(maphash (lambda (k v) (setq names (cons k names))) allnames)
		(setq fbg/all-group-names names)
		;; add buffers to frames
		(dolist (frame frames)
			(let ((name (fbg/frame-group frame)))
				(when name
					(fbg/attach-buffers-to-frame frame name bufs)))))
	(message "FrameBufferGroup restored"))

(defun fbg/attach-buffers-to-frame (frame name &optional allbufs)
	;; add buffers in group to frame
	(let ((bufs (seq-filter
							 (lambda (b)
								 (with-current-buffer b
									 (member name frame-buffer-group-groups-buffer-in)))
							 (or allbufs (fbg/builtin-buffer-list)))))
		(when bufs
			(fbg/add-buffers-to-frame frame bufs))))

(defun frame-buffer-group-remove-current-buffer ()
	"Remove current buffer from this group."
  (interactive)
	(let* ((frame (selected-frame))
				 (name (fbg/frame-group frame))
				 (buf (current-buffer))
				 (frames (or (fbg/frames-in-group name) (list frame))))
		(dolist (frame frames)
			;; make sure buf is not displayed in frame
			(dolist (w (window-list frame))
				(when (eq buf (window-buffer w))
					(with-selected-window w (switch-to-buffer "*scratch*"))))
			;; remove buf from frame
			(fbg/reset-frame-buffer-list frame (delete buf (frame-parameter frame 'buffer-list))))
		(fbg/remove-buffer-from-group name)))

(defun fbg/frames-in-group (name)
	(when (member name fbg/all-group-names)
		(seq-filter
		 (lambda (x) (equal name (fbg/frame-group x)))
		 (frame-list))))

(defun frame-buffer-group-add-buffers-from-buffer-menu ()
  (interactive)
  (let* ((newbufs (Buffer-menu-marked-buffers))
				 (frame (selected-frame))
				 (name (fbg/frame-group frame))
				 (allframes (or (fbg/frames-in-group name) (list frame))))
		(dolist (frame allframes)
			(fbg/add-buffers-to-frame frame newbufs))
		(fbg/update-groups-buffer-in frame name)))

(defun frame-buffer-group-switch-to-buffer ()
  (interactive)
  (fbg/call-interactively-with-builtin-buffer-list 'switch-to-buffer))

(defun fbg/after-make-frame (frame)
	;; don't put current buffer in the new frame
	(let ((w (car (window-list frame))))
		(with-selected-window w (switch-to-buffer "*scratch*")))
	(fbg/reset-frame-buffer-list frame nil)
	(set-frame-parameter frame 'title nil)
	(fbg/add-stick-buffers frame))

(defun fbg/add-buffers-to-frame (frame newbufs)
	(let ((bufs (frame-parameter frame 'buffer-list)))
		(dolist (b (mapcar (lambda (x) (get-buffer x)) newbufs))
			(setq bufs (fbg/add-to-list b bufs)))
		(fbg/reset-frame-buffer-list frame bufs)))

(defun fbg/reset-frame-buffer-list (frame bufs)
	(set-frame-parameter frame 'buffer-list bufs))

(defun fbg/add-stick-buffers (frame)
	(fbg/add-buffers-to-frame frame '("*scratch*" "*Messages*")))

(defun fbg/buffer-list (&optional frame)
	(if (or (not frame-buffer-group-mode)
					(eq major-mode 'Buffer-menu-mode))
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

(defun fbg/enable ()
	(message "fbg/enable")
	(advice-add 'buffer-list :override 'fbg/buffer-list)
	(add-hook 'after-make-frame-functions 'fbg/after-make-frame)
	(add-hook 'focus-in-hook 'fbg/merge-buffers)
	(add-hook 'focus-out-hook 'fbg/ensure-state)
	(when (and (boundp 'desktop-locals-to-save)
						 (not (memq 'frame-buffer-group-groups-buffer-in desktop-locals-to-save)))
		(customize-set-variable 'desktop-locals-to-save
														(cons 'frame-buffer-group-groups-buffer-in desktop-locals-to-save)))
	(when (boundp 'desktop-after-read-hook)
		(add-hook 'desktop-after-read-hook 'fbg/start-restore))
	(setq fbg/timer
				(run-with-idle-timer 29 t 'fbg/ensure-buffers-in-group))
	;; use builtin for some functions
	(dolist (func '(desktop-save))
		(when (fboundp func)
			(advice-add func :around 'fbg/apply-with-builtin-buffer-list))))

(defun fbg/disable ()
	(message "fbg/disable")
	(advice-remove 'buffer-list 'fbg/buffer-list)
	(when fbg/timer
		(cancel-timer fbg/timer)
		(setq fbg/timer nil))
	(remove-hook 'after-make-frame-functions 'fbg/after-make-frame)
	(remove-hook 'focus-in-hook 'fbg/merge-buffers)
	(remove-hook 'focus-out-hook 'fbg/ensure-state)
	(when (and (boundp 'desktop-locals-to-save)
						 (memq 'frame-buffer-group-groups-buffer-in desktop-locals-to-save))
		(customize-set-variable 'desktop-locals-to-save
														(seq-remove (lambda (x) (eq x 'frame-buffer-group-groups-buffer-in))
																				desktop-locals-to-save)))
	(when (boundp 'desktop-after-read-hook)
		(remove-hook 'desktop-after-read-hook 'fbg/restore))
	(dolist (func '(desktop-save))
		(when (fboundp func)
			(advice-remove func 'fbg/apply-with-builtin-buffer-list))))

(provide 'frame-buffer-group)
;;; frame-buffer-group.el ends here
