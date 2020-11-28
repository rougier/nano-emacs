;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - Nicolas .P Rougier
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
;; This file defines the help message and bind it to M-h
;; [C-x C-f] Open  [⌘-w] Copy   [C-w] Cut   [C-s] Search  [C-g]   Cancel
;; [C-x C-s] Save  [C-y] Paste  [C-/] Undo  [⌘-x] Command [C-x C-c] Quit
;;
;; Usage:
;;
;;  (require 'nano-help)
;;  M-: (nano-help) or M-h
;;
;; ---------------------------------------------------------------------

(defun nano-help ()
  (interactive)
  (let ((message-log-max nil))
    (message
     (concat
      " [C-x C-f] Open  [⌘-w] Copy   [C-w] Cut   [C-s] Search           "
      (propertize "[C-g]   Cancel" 'face 'bold)
      "\n"
      " [C-x C-s] Save  [C-y] Paste  [C-/] Undo  [⌘-x] Command          "
      (propertize "[C-x C-c] Quit" 'face 'bold)))
    (sit-for 30)))

(setq mac-pass-command-to-system nil)
(global-set-key (kbd "M-h") 'nano-help)

(provide 'nano-help)


