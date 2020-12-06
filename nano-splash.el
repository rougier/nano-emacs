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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------
;; 
;; This file defines the splash screen
;;  - No logo, no modeline, no scrollbars
;;  - Any key / mouse click kills the splash screen
;;  - With emacs-mac (Mituharu), splash screen is faded out after .5 seconds
;;
;; Note: The screen is not shown if there are opened file buffers. For
;;       example, if you start emacs with a filename on the command
;;       line, the splash screen is not shown.
;;
;; Usage:
;;  (require 'nano-splash)
(require 'subr-x)
(require 'cl-lib)

(defun nano-splash ()
  "Nano Emacs splash screen"
  
  (interactive)

  ;; Hide modeline before window-body-height is computed
  (let* ((splash-buffer (get-buffer-create "*splash*")))
    (with-current-buffer splash-buffer
      (setq header-line-format nil)
      (setq mode-line-format nil)))
  
  (let* ((splash-buffer  (get-buffer-create "*splash*"))
         (height         (round (- (window-body-height nil) 1) ))
         (width          (round (window-body-width nil)        ))
         (padding-center (+ (/ height 2) 1)))
    
    ;; If there are buffer associated with filenames,
    ;;  we don't show the splash screen.
    (if (eq 0 (length (cl-loop for buf in (buffer-list)
                              if (buffer-file-name buf)
                              collect (buffer-file-name buf))))
        
        (with-current-buffer splash-buffer
          (erase-buffer)
          
          ;; Buffer local settings
          (if (one-window-p) (setq mode-line-format nil))
          (setq cursor-type nil)
          (setq line-spacing 0)
          (setq vertical-scroll-bar nil)
          (setq horizontal-scroll-bar nil)
          (setq fill-column width)
          (face-remap-add-relative 'link :underline nil)
          (if (not (display-graphic-p)) (menu-bar-mode 0))

          ;; Vertical padding to center
          (insert-char ?\n padding-center)
          (insert (propertize "GNU Emacs / N Λ N O" 'face 'nano-face-strong))
          (center-line)
          (insert "\n")
          (insert (propertize "Emacs made simple" 'face 'nano-face-faded))
          (center-line)

          (goto-char 0)
          (read-only-mode t)
          (local-set-key [t] 'nano-splash-kill)
          (display-buffer-same-window splash-buffer nil)
          (run-with-idle-timer 0.05 nil (lambda() (message nil)))
          (run-with-idle-timer 0.50 nil 'nano-splash-fade-out-slow)
	  (if (fboundp 'nano-splash-help-message)
              (run-with-idle-timer 0.55 nil 'nano-splash-help-message))
	  )
      (nano-splash-kill))))


(defun center-string (string)
  "Pad a string with space on the left such as to center it"
  (let* ((padding (/ (- (window-body-width) (length string)) 2))
         (padding (+ (length string) padding)))
    ;; If the string is displayed as a tooltip, don't pad it
    (if (and tooltip-mode (fboundp 'x-show-tip))
        string
      (format (format "%%%ds" padding) string))))

;; Mac only animation , available from
;;  https://bitbucket.org/mituharu/emacs-mac/src/master/
;;  https://github.com/railwaycat/homebrew-emacsmacport
(defvar mac-animation-locked-p nil)
(defun mac-animation-toggle-lock ()
  (setq mac-animation-locked-p (not mac-animation-locked-p)))
(defun mac-animation-fade-out (duration &rest args)
  (unless mac-animation-locked-p
    (mac-animation-toggle-lock)
    (mac-start-animation nil :type 'fade-out :duration duration)
    (run-with-timer duration nil 'mac-animation-toggle-lock)))
(defun nano-splash-fade-out (duration)
  "Fade out current frame for duration and goes to command-or-bufffer"
  (interactive)
  (defalias 'mac-animation-fade-out-local
    (apply-partially 'mac-animation-fade-out duration))
  (if (get-buffer "*splash*")
      (progn (if (and (display-graphic-p) (fboundp 'mac-start-animation))
                 (advice-add 'set-window-buffer
                             :before 'mac-animation-fade-out-local))
             (message nil)
             (kill-buffer "*splash*")
             (if (and (display-graphic-p) (fboundp 'mac-start-animation))
                 (advice-remove 'set-window-buffer
                                'mac-animation-fade-out-local)))))
(defun nano-splash-fade-out-slow ()
  (interactive) (nano-splash-fade-out 1.00))
(defun nano-splash-fade-out-fast ()
  (interactive) (nano-splash-fade-out 0.25))

(defun nano-splash-kill ()
  "Kill the splash screen buffer (immediately)."
  (interactive)
  (if (get-buffer "*splash*")
      (progn (message nil)
             (cancel-function-timers 'nano-splash-fade-out-slow)
             (cancel-function-timers 'nano-spash-help-message)
             (kill-buffer "*splash*"))))


;; Install hook after frame parameters have been applied and only if
;; no option on the command line
(if (and (not (member "-no-splash"  command-line-args))
         (not (member "--file"      command-line-args))
         (not (member "--insert"    command-line-args))
         (not (member "--find-file" command-line-args))
         ;; (not inhibit-startup-screen)
         )
    (progn
      (add-hook 'window-setup-hook 'nano-splash)
      (setq inhibit-startup-screen t 
            inhibit-startup-message t
            inhibit-startup-echo-area-message t)))


(provide 'nano-splash)


