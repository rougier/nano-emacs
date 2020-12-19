;; -*- lexical-binding: t -*-
;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------
;; An experimental header command prompt for entering non intrusive
;; short text, without auto-completion nor history. Typical usage is
;; for one line org-captures (TODO or meeting) or short mail
;; answers. The command prompt is displayed using the header line and
;; the content is copied from a tiny one line window at the bottom of
;; this window. I took care of setting the text and cursor color to
;; white such that they are mostly invisible.
;; ---------------------------------------------------------------------
(define-minor-mode nano-command-mode
  "Nano command mode"
  :keymap  (make-sparse-keymap))

(defface nano-face-command nil
  "Face for header line command"
  :group 'nano)

(defface nano-face-command-prompt nil
  "Face for header line command prompt"
  :group 'nano)

(defface nano-face-command-cursor nil
  "Face for header line command cursor"
  :group 'nano)



(set-face-attribute 'nano-face-command nil
                    :foreground nano-color-foreground
                    :background nano-color-subtle
                    :box `(:line-width 1
                           :color ,nano-color-foreground
                           :style nil)
                   :inherit nil)

(set-face-attribute 'nano-face-command-prompt nil
                    :inherit 'nano-face-strong
                    :foreground nano-color-background
                    :background nano-color-foreground
                    :box `(:line-width 1
                           :color ,nano-color-foreground
                           :style nil))

(set-face-attribute 'nano-face-command-cursor nil
                    :foreground nano-color-background
                    :background nano-color-foreground)

(defun nano-command-update ()
  (interactive)
  (if (window-live-p (get-buffer-window "*nano-command*"))
      (with-current-buffer "*nano-command*"
        (let ((eol (save-excursion (goto-char (point-min)) (point-at-eol))))
          (if (> (point) eol) (goto-char eol)))))
  (force-mode-line-update t))

(defun nano-command-cancel (master slave cookie)
  ;; Close master
  (with-selected-window (get-buffer-window master)
    (delete-window))

  ;; Restore slave to normal state
  (with-selected-window (get-buffer-window slave)
    (kill-local-variable 'header-line-format)
    (face-remap-remove-relative cookie))
  
  ;; Update
  (force-mode-line-update t))


(defun nano-command (prompt &optional content information)

  (setq nano-command-cookie
        (face-remap-add-relative 'header-line 'nano-face-command))
  (setq nano-command-master
        "*nano-command*")
  (setq nano-command-slave
        (current-buffer))
  (setq nano-command-information
        information)

  (let ((window-min-height 1)
        (window-safe-min-height 1)
        (window-resize-pixelwise t)
        (split-window-keep-point t)
        (split-window-keep-point t))
    (with-selected-window (split-window-vertically -2)
      (switch-to-buffer (get-buffer-create nano-command-master))
      (erase-buffer)
      (org-mode)
      (nano-command-mode)
      (if content (insert content))
      (insert "\n")
      (insert "-")
      (goto-char (point-min))

      (overlay-put (make-overlay (point-at-bol) (+ (point-at-eol) 1))
                   'face '(:height 10))

      (goto-char (point-min))
      (setq header-line-format nil)
      (setq mode-line-format nil)
      (face-remap-add-relative 'default `(:foreground ,nano-color-background))
      (face-remap-add-relative 'region  `(:background ,nano-color-background))
      (setq cursor-type nil)
      (fit-window-to-buffer)
      (setq window-size-fixed t)))

  (setq header-line-format
      (list
       (propertize " "  'face 'nano-face-command-prompt
		        'display `(raise -0.20))
       (propertize prompt 'face 'nano-face-command-prompt)
       (propertize " "  'face 'nano-face-command-prompt
		        'display `(raise +0.15))
       (propertize " " )
       `(:eval
         (let* ((master "*nano-command*")                
                (content (with-current-buffer master
                           (save-excursion
                             (goto-char (point-min))
                             (buffer-substring (point-at-bol) (point-at-eol)))))
                (content (if (> (length content) 0)
                             content
                           (propertize ,information
                                       'face 'nano-face-faded)))
                (content (concat content " "))
                (point  (with-current-buffer master (point)))
                (region (with-current-buffer master (region-active-p))))
           ;; Cursor 
           (put-text-property (- point 1) point
                              'face 'nano-face-command-cursor content)
           ;; Region
           (if region
               (let ((beg (with-current-buffer master (region-beginning)))
                     (end (with-current-buffer master (region-end))))
                 (put-text-property (- beg 1) (- end 1)
                                    'face 'region content)))
           content))))

  (with-current-buffer nano-command-master
    (add-hook 'post-command-hook 'nano-command-update nil t)
    (if (display-graphic-p)
        (define-key nano-command-mode-map (kbd "ESC")
                (lambda () (interactive) (nano-command-cancel
                                nano-command-master
                                nano-command-slave
                                nano-command-cookie))))
    (define-key nano-command-mode-map (kbd "C-g")
      (lambda () (interactive) (nano-command-cancel
                                nano-command-master
                                nano-command-slave
                                nano-command-cookie)))
    (define-key nano-command-mode-map (kbd "RET")
      (lambda () (interactive) (nano-command-cancel
                                nano-command-master
                                nano-command-slave
                                nano-command-cookie))))
  (nano-command-update)
  (select-window (get-buffer-window nano-command-master)))


;; ---------------------------------------------------------------------
(defun nano-command-x ()
  (interactive)
  (nano-command "M-x" "" "Enter command"))

(defun nano-command-shell ()
  (interactive)
  (nano-command ">_" "" "Enter shell command"))

(defun nano-command-capture-todo ()
  (interactive)
  (nano-command "TODO" "" "Enter TODO item"))

(defun nano-command-capture-meeting ()
  (interactive)
  (nano-command "ORG" "" "New meeting"))

(defun nano-command-mail-reply ()
  (interactive)
  (nano-command "MAIL" "" "Reply to sender"))

(defun nano-command-mail-reply-all ()
  (interactive)
  (nano-command "MAIL" "" "Reply to all"))


(define-key global-map (kbd "C-c x") #'nano-command-x)
(define-key global-map (kbd "C-c s") #'nano-command-shell)
(define-key global-map (kbd "C-c t") #'nano-command-capture-todo)
(define-key global-map (kbd "C-c m") #'nano-command-capture-meeting)
(define-key global-map (kbd "C-c r") #'nano-command-mail-reply)

