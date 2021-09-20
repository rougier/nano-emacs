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
;;(require 'nano-colors)

(setq default-frame-alist
      (append (list
           '(font . "Roboto Mono:style=Light:size=14")
	       '(min-height . 1) '(height    . 45)
	       '(min-width  . 1) '(width      . 81)
           '(vertical-scroll-bars . nil)
           '(internal-border-width . 1)
           '(left-fringe    . 24)
           '(right-fringe   . 24)
           '(tool-bar-lines . 0)
           '(menu-bar-lines . 0))))


(set-face-attribute 'header-line nil
                    :foreground nano-light-foreground
                    :background nano-light-subtle
                    :underline nil
                    :box `(:line-width 4
                           :color ,nano-light-background
                           :style nil))

(set-face-attribute 'internal-border nil :background nano-light-foreground)
(set-face-foreground 'vertical-border nano-light-subtle)
(window-divider-mode 0)

(defun nano-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((prefix (cond ((string= status "**")
                        (propertize "[M] "
                                    'face `(:inherit bold
                                            :foreground ,nano-light-foreground)))
                       ((string= status "RO")
                        (propertize "[R] "
                                    'face `(:inherit bold
                                            :foreground ,nano-light-foreground)))
                       (t
                        (propertize ""
                                    'face `(
                                            :foreground ,nano-light-foreground)))))
         (name (propertize name 'face `(:inherit bold)))
         (left (concat
                (propertize " " 'face nil 'display '(raise -0.30))
                prefix
                name
                (propertize " " 'face nil 'display '(raise +0.25))
                ;; " "
                primary))
         (right (concat (propertize secondary
                                    'face `(:foreground ,nano-light-faded))
                        (propertize " " 'face nil)))
         (available-width (- (window-total-width nil 'floor)
                             (if (display-graphic-p) 1 0)
                             (length left) (length right)))
	 (available-width (max 1 available-width)))
    (concat left (make-string available-width ?\ ) right)))

(provide 'nano-compact)

