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
(defun nano-theme-set-light ()
  "Apply light Nano theme base."
  ;; Colors from Material design at https://material.io/
  (setq frame-background-mode    'light)
  (setq nano-color-foreground "#37474F") ;; Blue Grey / L800
  (setq nano-color-background "#FFFFFF") ;; White
  (setq nano-color-highlight  "#FAFAFA") ;; Very Light Grey
  (setq nano-color-critical   "#FF6F00") ;; Amber / L900
  (setq nano-color-salient    "#673AB7") ;; Deep Purple / L500
  (setq nano-color-strong     "#000000") ;; Black
  (setq nano-color-popout     "#FFAB91") ;; Deep Orange / L200
  (setq nano-color-subtle     "#ECEFF1") ;; Blue Grey / L50
  (setq nano-color-faded      "#B0BEC5") ;; Blue Grey / L200
  )
(nano-theme-set-light)

(provide 'nano-theme-light)
