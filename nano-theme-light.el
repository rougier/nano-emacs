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
(setq frame-background-mode   'light)

(defvar nano-color-foreground "#37474F")
(defvar nano-color-background "#FFFFFF")
(defvar nano-color-highlight  "#FAFAFA")
(defvar nano-color-critical   "#FF6F00")
(defvar nano-color-salient    "#673AB7")
(defvar nano-color-strong     "#000000")
(defvar nano-color-popout     "#FFAB91")
(defvar nano-color-subtle     "#ECEFF1")
(defvar nano-color-faded      "#B0BEC5")

(require 'nano-theme)
(provide 'nano-theme-light)
