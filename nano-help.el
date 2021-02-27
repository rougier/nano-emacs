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
;; This file defines a help message (M-p) and a help scren (M-h)
;;
;; Usage:
;;
;;  (require 'nano-help)
;;  M-: (nano-quick-help) or M-p
;;  M-: (nano-help) or M-h
;;
;; ---------------------------------------------------------------------

;; Help message
(defun nano-quick-help ()
  (interactive)
  (let ((message-log-max nil))
    (message
     (concat
      (propertize "\n" 'face '(:height 0.4))
      " [C-x C-f] Open  [M-w] Copy   [C-w] Cut   [C-s] Search           "
      (propertize "[C-g]   Cancel" 'face 'bold)
      "\n"
      " [C-x C-s] Save  [C-y] Paste  [C-/] Undo  [M-x] Command          "
      (propertize "[C-x C-c] Quit" 'face 'bold)
      (propertize "\n " 'face '(:height 0.5))))
    (sit-for 30)))

;; Help screen
(define-derived-mode nano-help-mode org-mode "Nano help mode")
(define-key nano-help-mode-map (kbd "q") #'kill-current-buffer)
(defun nano-help ()
  (interactive)
  (find-file-read-only (locate-file "quick-help.org" load-path))
  (nano-help-mode)
  (setq-local org-confirm-elisp-link-function nil))

;; Bindings for quick help and help
(setq mac-pass-command-to-system nil)
(global-set-key (kbd "M-p") 'nano-quick-help)
(global-set-key (kbd "M-h") 'nano-help)

(defun nano-splash-help-message ()
  (message
   (concat
    (if (display-graphic-p) (propertize "\n " 'face '(:height 0.4)))
    (propertize (concat
                 "Type M-p for quick help, M-h for help."
                 " M stands for Alt, Command or (Esc)ape.")
                'face 'nano-face-faded)
    (if (display-graphic-p) (propertize "\n " 'face '(:height 0.5))))))

(provide 'nano-help)
