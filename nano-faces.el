;;; nano-faces --- Face settings for nano-emacs
;;; License:
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
;;; Commentary:
;;
;; This file defines the 6 basic nano faces:
;;
;; - nano-face-critical  - nano-face-popout   - nano-face-salient
;; - nano-face-default   - nano-face-faded    - nano-face-subtle
;;
;; Several nano modules require
;;
;; ---------------------------------------------------------------------
;;; Code:

(require 'nano-base-colors)



(defcustom nano-font-family-monospaced "Roboto Mono"
  "Name of the font-family to use for nano.
Defaults to Roboto Mono. Customizing this might lead to conflicts
if the family does not have sufficient bold/light etc faces."
  :group 'nano
  :type 'string)

(defcustom nano-font-family-proportional nil
  "Font to use for variable pitch faces.
Setting this allows nano to display variable pitch faces when,
for instance, 'variable-pitch-mode' or 'mixed-pitch-mode' is active in a buffer.
Defaults to nil."
  :group 'nano
  :type 'string)

(defcustom nano-font-size 14
  "Default value for the font size of nano-theme in pt units.
Note: to change this after startup, call
\(nano-faces\) and \(nano-themes\)."
  :group 'nano
  :type 'integer)

;; A theme is fully defined by these seven faces

(defface nano-face-default nil
  "Default face is used for regular information."
  :group 'nano)

(defface nano-face-variable-pitch nil
  "Default variable-pitch face is used for variable pitch mode."
  :group 'nano)

(defface nano-face-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group 'nano)

(defface nano-face-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group 'nano)

(defface nano-face-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group 'nano)

(defface nano-face-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group 'nano)

(defface nano-face-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group 'nano)

(defface nano-face-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group 'nano)

(defface nano-face-header-default nil
  "Default face for ther header line."
  :group 'nano)

(defface nano-face-header-critical nil
  "Critical face for ther header line."
  :group 'nano)

(defface nano-face-header-popout nil
  "Popout face for ther header line."
  :group 'nano)

(defface nano-face-header-strong nil
  "Strong face for ther header line."
  :group 'nano)

(defface nano-face-header-salient nil
  "Salient face for ther header line."
  :group 'nano)

(defface nano-face-header-faded nil
  "Faded face for ther header line."
  :group 'nano)

(defface nano-face-header-subtle nil
  "Subtle face for ther header line."
  :group 'nano)

(defface nano-face-header-highlight nil
  "Highlight face for ther header line."
  :group 'nano)

(defface nano-face-header-separator nil
  "Face for separating item in the header line (internal use)"
  :group 'nano)

(defface nano-face-header-filler nil
  "Face compsenting spaces in the header line (internal use) "
  :group 'nano)

(defface nano-face-tag-default nil
  "Default face for tags"
  :group 'nano)

(defface nano-face-tag-faded nil
  "Faded face for tags"
  :group 'nano)

(defface nano-face-tag-strong nil
  "Strong face for tags"
  :group 'nano)

(defface nano-face-tag-salient nil
  "Salient face for tags"
  :group 'nano)

(defface nano-face-tag-popout nil
  "Popout face for tags"
  :group 'nano)

(defface nano-face-tag-critical nil
  "Critical face for tags"
  :group 'nano)

(defun nano-what-faces (pos)
  "Get the font faces at POS."
  (interactive "d")
  (let ((faces (remq nil
                     (list
                      (get-char-property pos 'read-face-name)
                      (get-char-property pos 'face)
                      (plist-get (text-properties-at pos) 'face)))))
    (message "Faces: %s" faces)))

(defun nano-faces ()
  "Derive face attributes for nano-faces using nano-theme values."
  (set-face-attribute 'nano-face-default nil
                      :foreground nano-color-foreground
                      :background nano-color-background
                      :family     nano-font-family-monospaced
                      :height       (* nano-font-size 10))
  (set-face-attribute 'nano-face-critical nil
                      :foreground nano-color-foreground
                      :background nano-color-critical)
  (set-face-attribute 'nano-face-popout nil
                      :foreground nano-color-popout)

  (set-face-attribute 'nano-face-variable-pitch nil
                          :foreground (face-foreground 'nano-face-default)
                          :background (face-background 'nano-face-default)
                          :family nano-font-family-proportional
                          :height (* nano-font-size 10))
  (if (display-graphic-p)
      (set-face-attribute 'nano-face-strong nil
                          :foreground nano-color-strong
                          :weight 'medium)
    (set-face-attribute 'nano-face-strong nil
                        :foreground nano-color-strong
                        :weight 'bold))

  (set-face-attribute 'nano-face-salient nil
                      :foreground nano-color-salient
                      :weight 'light)

  (set-face-attribute 'nano-face-faded nil
                      :foreground nano-color-faded
                      :weight 'light)

  (set-face-attribute 'nano-face-subtle nil
                      :background nano-color-subtle)

  (set-face-attribute 'nano-face-header-default nil
                      :foreground nano-color-foreground
                      :background nano-color-subtle
                      :box `(:line-width 1
                                         :color ,nano-color-background
                                         :style nil))

  (set-face-attribute 'nano-face-tag-default nil
                      :foreground nano-color-foreground
                      :background nano-color-background
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 nano-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,nano-color-foreground
                                         :style nil))

  (set-face-attribute 'nano-face-header-strong nil
                      :foreground nano-color-strong
                      :background nano-color-subtle
                      :inherit 'nano-face-strong
                      :box `(:line-width 1
                                         :color ,nano-color-background
                                         :style nil))

  (set-face-attribute 'nano-face-tag-strong nil
                      :foreground nano-color-strong
                      :background nano-color-subtle
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 nano-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,nano-color-strong
                                         :style nil))

  (set-face-attribute 'nano-face-header-salient nil
                      :foreground nano-color-background
                      :background nano-color-salient
                      :box `(:line-width 1
                                         :color ,nano-color-background
                                         :style nil))

  (set-face-attribute 'nano-face-tag-salient nil
                      :foreground nano-color-background
                      :background nano-color-salient
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 nano-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,nano-color-salient
                                         :style nil))

  (set-face-attribute 'nano-face-header-popout nil
                      :foreground nano-color-background
                      :background nano-color-popout
                      :box `(:line-width 1
                                         :color ,nano-color-background
                                         :style nil))

  (set-face-attribute 'nano-face-tag-popout nil
                      :foreground nano-color-background
                      :background nano-color-popout
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 nano-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,nano-color-popout
                                         :style nil))

  (set-face-attribute 'nano-face-header-faded nil
                      :foreground nano-color-background
                      :background nano-color-faded
                      :box `(:line-width 1
                                         :color ,nano-color-background
                                         :style nil))

  (set-face-attribute 'nano-face-tag-faded nil
                      :foreground nano-color-background
                      :background nano-color-faded
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 nano-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,nano-color-faded
                                         :style nil))

  (set-face-attribute 'nano-face-header-subtle nil)

  (set-face-attribute 'nano-face-header-critical nil
                      :foreground nano-color-background
                      :background nano-color-critical
                      :box `(:line-width 1
                                         :color ,nano-color-background
                                         :style nil))
  (set-face-attribute 'nano-face-tag-critical nil
                      :foreground nano-color-background
                      :background nano-color-critical
                      :weight 'regular
                      :height (if (display-graphic-p)
                                  (round
                                   (* 0.85 (* 10 nano-font-size)))
                                                1)
                      :box `(:line-width 1
                                         :color ,nano-color-critical
                                         :style nil))

  (set-face-attribute 'nano-face-header-separator nil
                      :inherit 'nano-face-default
                      :height 0.1)
  (set-face-attribute 'nano-face-header-filler nil
                      :inherit 'nano-face-header-default
                      :height 0.1)
  (set-face-attribute 'nano-face-header-highlight nil
                      :inherit 'nano-face-header-faded
                      :box nil))

(provide 'nano-faces)
;;; nano-faces.el ends here
