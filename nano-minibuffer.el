;;; -*- lexical-binding: t -*-
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
;; An experimental header command prompt for entering non intrusive
;; short text, without auto-completion nor history. Typical usage is
;; for one line org-captures (TODO or meeting) or short mail
;; answers. The command prompt is displayed using the header line and
;; the content is copied from a tiny one line window at the bottom of
;; this window. It takes care of setting the text to white and hiding
;; the cursor such that it is mostly invisible.
;; ---------------------------------------------------------------------
(require 'mini-frame)

(with-eval-after-load 'mini-frame

  ;; Miniframe at the bottom for a nicer display
  (setq mini-frame-show-parameters
        `((left . 0.5)
          (top . 1.0)
          (width . 1.0)
          (height . 5)
          (left-fringe . 12)
          (right-fringe .12)
          (child-frame-border-width . 0)
          (internal-border-width . 0)
          (foreground-color . ,nano-color-foreground)
          (background-color . ,nano-color-subtle)))

   (with-eval-after-load 'ivy
     (setq ivy-height 4)
     (set-face 'ivy-current-match 'nano-face-strong)
     ;; See https://github.com/abo-abo/swiper/issues/2383
     (setcdr (assoc t ivy-format-functions-alist) #'ivy-format-function-line)
     (set-face-attribute 'ivy-current-match nil
                         :extend t
                         :foreground nano-color-background
                         :background nano-color-faded))
   (setq mini-frame-ignore-commands
         '("edebug-eval-expression" debugger-eval-expression))
    
   (setq mini-frame-internal-border-color (material-color "blue-grey-2"))
   ;; (setq mini-frame-resize 'grow-only) ;; -> buggy as of 01/05/2021
   (setq mini-frame-resize 'not-set)
   ;; (setq mini-frame-resize nil)
   (add-hook 'minibuffer-setup-hook
             (lambda ()
               (overlay-put (make-overlay (point-min) (+ (point-min) 1))
                            'before-string
                            (propertize "\n" 'face `(:extend t
                                                             :height .5)))))
   )
  
(mini-frame-mode 1)
(provide 'nano-minibuffer)
