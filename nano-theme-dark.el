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
(setq frame-background-mode   'dark)

(defvar nano-color-foreground "#ECEFF4")
(defvar nano-color-background "#2E3440")
(defvar nano-color-highlight  "#3B4252")
(defvar nano-color-critical   "#EBCB8B")
(defvar nano-color-salient    "#81A1C1")
(defvar nano-color-strong     "#ECEFF4")
(defvar nano-color-popout     "#D08770")
(defvar nano-color-subtle     "#434C5E")
(defvar nano-color-faded      "#616E87")

(require 'nano-theme)
(provide 'nano-theme-dark)
