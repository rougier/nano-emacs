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
;; Defines the 9 basic colors of the nano theme.
;; The default values are loaded from well-known faces of Emacs.
;;
;; To change nano's appearance you therefore may:
;; - Load any Emacs theme before loading nano to change the appearance
;; - Load one of the few nano themes after this file. This will result inactive
;;   the best experience.
;; - Set your own colors by customizing nano group
;;
;; ---------------------------------------------------------------------

(defgroup nano '()
  "Faces and colors for the nano emacs theme")

;; Derive our default color set from classic Emacs faces.
;; This allows dropping nano components into already themed Emacsen with varying
;; degrees of visual appeal.
;;
;; We memorize the default colorset in this var in order not to confuse
;; customize: the STANDARD argument of defcustom gets re-evaluated by customize
;; to determine if the current value is default or not.
(defvar nano-base-colors--defaults
  `((foreground . ,(face-foreground 'default nil t))
    (background . ,(face-background 'default nil t))
    (highlight . ,(face-background 'fringe nil t))
    (critical . ,(face-foreground 'error nil t))
    (salient . ,(face-foreground 'font-lock-keyword-face nil t))
    (strong . ,(face-foreground 'default nil t))
    (popout . ,(face-foreground 'font-lock-string-face nil t))
    (subtle . ,(face-background 'mode-line-inactive nil t))
    (faded . ,(face-foreground 'shadow nil t))))

(defun nano-base-colors--get (name)
  "Get default color associated with symbol NAME."
  (cdr (assoc name nano-base-colors--defaults)))

(defcustom nano-color-foreground (nano-base-colors--get 'foreground)
  ""
  :type 'color
  :group 'nano)

(defcustom nano-color-background (nano-base-colors--get 'background)
  ""
  :type 'color
  :group 'nano)

(defcustom nano-color-highlight (nano-base-colors--get 'highlight)
  ""
  :type 'color
  :group 'nano)

(defcustom nano-color-critical (nano-base-colors--get 'critical)
  ""
  :type 'color
  :group 'nano)

(defcustom nano-color-salient (nano-base-colors--get 'salient)
  ""
  :type 'color
  :group 'nano)

(defcustom nano-color-strong (nano-base-colors--get 'strong)
  ""
  :type 'color
  :group 'nano)

(defcustom nano-color-popout (nano-base-colors--get 'popout)
  ""
  :type 'color
  :group 'nano)

(defcustom nano-color-subtle (nano-base-colors--get 'subtle)
  ""
  :type 'color
  :group 'nano)

(defcustom nano-color-faded (nano-base-colors--get 'faded)
  ""
  :type 'color
  :group 'nano)

(provide 'nano-base-colors)
