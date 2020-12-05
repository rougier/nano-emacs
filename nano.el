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

;; Path to nano emacs modules (mandatory)
(add-to-list 'load-path "/Users/rougier/Documents/GitHub/nano-emacs")
(add-to-list 'load-path ".")

;; Welcome message (optional)
(let ((inhibit-message t))
  (message "Welcome to GNU Emacs / N Λ N O edition")
  (message (format "Initialization time: %s" (emacs-init-time))))

;; Window layout (optional)
(require 'nano-layout)

;; Theme (mandatory)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(if (member "-dark" command-line-args)
    (require 'nano-theme-dark)
  (require 'nano-theme-light))

;; Nano defaults (optional)
(require 'nano-defaults)

;; Nano modeline (optional)
(require 'nano-modeline)

;; Nano bindings (optional)
(require 'nano-bindings)

;; Splash and help (optional)
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(unless (member "-no-splash" command-line-args)
  (require 'nano-help)
  (require 'nano-splash))

(provide 'nano)
