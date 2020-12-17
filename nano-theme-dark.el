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
(require 'nano-base-colors)

(defun nano-theme-set-dark ()
  "Apply dark Nano theme base."
  ;; Colors from Nord theme at https://www.nordtheme.com
  (setq frame-background-mode     'dark)
  (setq nano-color-foreground "#ECEFF4") ;; Snow Storm 3  / nord  6
  (setq nano-color-background "#2E3440") ;; Polar Night 0 / nord  0
  (setq nano-color-highlight  "#3B4252") ;; Polar Night 1 / nord  1
  (setq nano-color-critical   "#EBCB8B") ;; Aurora        / nord 11
  (setq nano-color-salient    "#81A1C1") ;; Frost         / nord  9
  (setq nano-color-strong     "#ECEFF4") ;; Snow Storm 3  / nord  6
  (setq nano-color-popout     "#D08770") ;; Aurora        / nord 12
  (setq nano-color-subtle     "#434C5E") ;; Polar Night 2 / nord  2
  (setq nano-color-faded      "#677691") ;;
  )

(nano-theme-set-dark)

(provide 'nano-theme-dark)
