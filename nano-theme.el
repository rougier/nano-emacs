;;; nano-theme --- Face settings for nano-emacs
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
;; This file derives all faces in various modes from the fundamental faces
;; defined in nano-faces.
;;
;; Notable exceptions:
;; - The highlight-line face uses a very light color.
;; - The terminal faces use a palette derived from material theme.
;; that uses a very light color,
;;
;; ---------------------------------------------------------------------
;;; Code:

(require 'nano-faces)

;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset FACE and make it inherit STYLE."
  (set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style))


(defun nano-theme--basics ()
  "Derive basic Emacs faces from nano-faces and nano-color-theme."

  (set-foreground-color nano-color-foreground)
  (set-background-color nano-color-background)

  ;; XXX the following seems to be a no-op, should it be removed?
  (set-face-attribute 'default nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)
                      :weight     'light
                      :family     (face-attribute 'nano-face-default :family)
                      :height     (face-attribute 'nano-face-default :height))

  (if (display-graphic-p)
      (set-face-attribute 'bold nil :weight 'regular)
    (set-face-attribute 'bold nil :weight 'bold))

  ;; Structural
  (set-face 'bold                                     'nano-face-strong)
  (set-face 'italic                                    'nano-face-faded)
  (set-face 'bold-italic                              'nano-face-strong)
  (set-face 'region                                   'nano-face-subtle)
  (set-face 'highlight                                'nano-face-subtle)
  ;;(set-face 'fixed-pitch                                     'default)
  (set-face 'fixed-pitch-serif                       'nano-face-default)
  (set-face 'cursor                                  'nano-face-default)
  (if 'nano-font-family-proportional
      (set-face-attribute 'variable-pitch nil ;; to work with mixed-pitch
                :foreground (face-foreground 'default)
                :background (face-background 'default)
                :family     (face-attribute 'nano-face-variable-pitch :family)
                :height     (face-attribute 'nano-face-variable-pitch :height))
      (set-face 'variable-pitch                     'nano-face-default))

  (set-face-attribute 'cursor nil
                      :background (face-foreground 'nano-face-default))
  (set-face-attribute 'window-divider nil
                      :foreground (face-background 'nano-face-default))
  (set-face-attribute 'window-divider-first-pixel nil
                      :foreground nano-color-background)
  ;;                  :foreground (face-background 'nano-face-subtle))
  (set-face-attribute 'window-divider-last-pixel nil
                      :foreground nano-color-background)
  ;;                  :foreground (face-background 'nano-face-subtle)))
  (set-face-foreground 'vertical-border nano-color-subtle)

  ;; Semantic
  (set-face 'shadow                                    'nano-face-faded)
  (set-face 'success                                 'nano-face-salient)
  (set-face 'warning                                  'nano-face-popout)
  (set-face 'error                                  'nano-face-critical)
  (set-face 'match                                    'nano-face-popout)

  ;; General
  (set-face 'buffer-menu-buffer                       'nano-face-strong)
  (set-face 'minibuffer-prompt                        'nano-face-strong)
  (set-face 'link                                    'nano-face-salient)
  (set-face 'fringe                                    'nano-face-faded)
  (set-face-attribute 'fringe nil
                      :foreground (face-background 'nano-face-subtle)
                      :background (face-background 'default))
  (set-face 'isearch                                  'nano-face-strong)
  (set-face 'isearch-fail                              'nano-face-faded)
  (set-face 'lazy-highlight                           'nano-face-subtle)
  (set-face 'trailing-whitespace                      'nano-face-subtle)
  (set-face 'show-paren-match                         'nano-face-popout)
  (set-face 'show-paren-mismatch                           'face-normal)
  (set-face-attribute 'tooltip nil                         :height 0.85)
  (set-face 'secondary-selection                      'nano-face-subtle)
  (set-face 'completions-common-part                   'nano-face-faded)
  (set-face 'completions-first-difference            'nano-face-default))
(defun nano-theme--font-lock ()
  "Derive font-lock faces from nano-faces."
  (set-face 'font-lock-comment-face                    'nano-face-faded)
  (set-face 'font-lock-doc-face                        'nano-face-faded)
  (set-face 'font-lock-string-face                    'nano-face-popout)
  (set-face 'font-lock-constant-face                 'nano-face-salient)
  (set-face 'font-lock-warning-face                   'nano-face-popout)
  (set-face 'font-lock-function-name-face             'nano-face-strong)
  (set-face 'font-lock-variable-name-face             'nano-face-strong)
  (set-face 'font-lock-builtin-face                  'nano-face-salient)
  (set-face 'font-lock-type-face                     'nano-face-salient)
  (set-face 'font-lock-keyword-face                  'nano-face-salient))


(defun nano-theme--mode-line ()
  "Derive mode-line and header-line faces from nano-faces."
  (set-face-attribute 'mode-line nil
                      :height 0.1
                      :foreground (if (display-graphic-p)
                                      (face-background 'nano-face-default)
                                    (face-foreground 'nano-face-default))
                      :background (face-background 'nano-face-default)
                      :underline  (if (display-graphic-p)
                                      (face-background 'nano-face-subtle)
                                    t)
                      :overline nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :height 0.1
                      :foreground (if (display-graphic-p)
                                      (face-background 'nano-face-default)
                                    (face-foreground 'nano-face-default))
                      :background (face-background 'nano-face-default)
                      :underline (if (display-graphic-p)
                                     (face-background 'nano-face-subtle)
                                   t)
                      :overline nil
                      :inherit nil
                      :box nil)
  
  ;;(when (display-graphic-p)
  (set-face-attribute 'header-line nil
                       :weight 'light
                       :foreground (face-foreground 'nano-face-default)
                       :background (face-background 'nano-face-default)

                       :overline nil
                       :underline nil
                       :box nil
                       :box `(:line-width 1
                                          :color ,(face-background 'nano-face-default)
                                          :style nil)
                       :inherit nil)

  ;; (when (not (display-graphic-p))
  ;;   (set-face-attribute 'header-line nil
  ;;                    :weight 'light
  ;;                       :foreground (face-foreground 'nano-face-default)
  ;;                       :background (face-background 'nano-face-subtle)
  ;;                       :inverse-video t
  ;;                       :overline nil
  ;;                       :underline nil
  ;;                       :box nil
  ;;                            :inherit nil))

  ;; (set-face-attribute 'internal-border nil
  ;;                     :background (face-foreground 'nano-face-default))

  (set-face-attribute 'internal-border nil
                       :background (face-background 'nano-face-default)))


(defun nano-theme--minibuffer ()
  "Derive minibuffer / echo area faces from nano faces."
  ;; Minibuffer / echo area
  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'nano-face-faded)))))


(defun nano-theme--hl-line ()
  "Derive hl-line faces from nano faces."
  (with-eval-after-load 'hl-line
    (set-face-attribute 'hl-line nil
                         :background nano-color-highlight)))



(defun nano-theme--buttons ()
  "Derive button faces from nano faces."
  ;; Buttons
  (with-eval-after-load 'cus-edit
    (set-face-attribute 'custom-button nil
                         :foreground (face-foreground 'nano-face-faded)
                         :background (face-background 'nano-face-default)
                         :box `(:line-width 1
                                :color ,(face-foreground 'nano-face-faded)
                                :style nil))
    (set-face-attribute 'custom-button-mouse nil
                         ;;                      :inherit 'custom-button
                         :foreground (face-foreground 'nano-face-faded)
                         :background (face-background 'nano-face-subtle)
                         :box `(:line-width 1
                                            :color ,(face-foreground 'nano-face-faded)
                                            :style nil))
    (set-face-attribute 'custom-button-pressed nil
                         :foreground (face-background 'default)
                         :background (face-foreground 'nano-face-salient)
                         :inherit 'nano-face-salient
                         :box `(:line-width 1
                                            :color ,(face-foreground 'nano-face-salient)
                                            :style nil)
                         :inverse-video nil)))


(defun nano-theme--info ()
  "Derive info faces from nano faces."
  (with-eval-after-load 'info
    (set-face 'info-menu-header                       'nano-face-strong)
    (set-face 'info-header-node                      'nano-face-default)
    (set-face 'info-index-match                      'nano-face-salient)
    (set-face 'Info-quoted                             'nano-face-faded)
    (set-face 'info-title-1                           'nano-face-strong)
    (set-face 'info-title-2                           'nano-face-strong)
    (set-face 'info-title-3                           'nano-face-strong)
    (set-face 'info-title-4                           'nano-face-strong)))


(defun nano-theme--speedbar ()
  "Derive speedbar faces from nano faces "
  (with-eval-after-load 'speedbar
    (set-face 'speedbar-button-face                    'nano-face-faded)
    (set-face 'speedbar-directory-face                'nano-face-strong)
    (set-face 'speedbar-file-face                    'nano-face-default)
    (set-face 'speedbar-highlight-face             'nano-face-highlight)
    (set-face 'speedbar-selected-face                 'nano-face-subtle)
    (set-face 'speedbar-separator-face                 'nano-face-faded)
    (set-face 'speedbar-tag-face                       'nano-face-faded)))


(defun nano-theme--bookmark ()
  "Derive bookmark faces from nano faces."
  (with-eval-after-load 'bookmark
    (set-face 'bookmark-menu-heading                  'nano-face-strong)
    (set-face 'bookmark-menu-bookmark                'nano-face-salient)))


(defun nano-theme--message ()
  "Derive message faces from nano faces."
  (with-eval-after-load 'message
    (unless (version< emacs-version "27.0")
      (set-face 'message-cited-text-1                  'nano-face-faded)
      (set-face 'message-cited-text-2                  'nano-face-faded)
      (set-face 'message-cited-text-3                  'nano-face-faded)
      (set-face 'message-cited-text-4                 'nano-face-faded))
    (set-face 'message-cited-text                      'nano-face-faded)
    (set-face 'message-header-cc                     'nano-face-default)
    (set-face 'message-header-name                    'nano-face-strong)
    (set-face 'message-header-newsgroups             'nano-face-default)
    (set-face 'message-header-other                  'nano-face-default)
    (set-face 'message-header-subject                'nano-face-salient)
    (set-face 'message-header-to                     'nano-face-salient)
    (set-face 'message-header-xheader                'nano-face-default)
    (set-face 'message-mml                            'nano-face-popout)
    (set-face 'message-separator                       'nano-face-faded)))


(defun nano-theme--outline ()
  "Derive outline faces from nano faces."
  (with-eval-after-load 'outline
    (set-face 'outline-1                              'nano-face-strong)
    (set-face 'outline-2                              'nano-face-strong)
    (set-face 'outline-3                              'nano-face-strong)
    (set-face 'outline-4                              'nano-face-strong)
    (set-face 'outline-5                              'nano-face-strong)
    (set-face 'outline-6                              'nano-face-strong)
    (set-face 'outline-7                              'nano-face-strong)
    (set-face 'outline-8                              'nano-face-strong)))


(defun nano-theme--customize ()
  "Derive customize faces from nano faces."
  (with-eval-after-load 'cus-edit
    (set-face 'widget-field                           'nano-face-subtle)
    (set-face 'widget-button                          'nano-face-strong)
    (set-face 'widget-single-line-field               'nano-face-subtle)
    (set-face 'custom-group-subtitle                  'nano-face-strong)
    (set-face 'custom-group-tag                       'nano-face-strong)
    (set-face 'custom-group-tag-1                     'nano-face-strong)
    (set-face 'custom-comment                          'nano-face-faded)
    (set-face 'custom-comment-tag                      'nano-face-faded)
    (set-face 'custom-changed                        'nano-face-salient)
    (set-face 'custom-modified                       'nano-face-salient)
    (set-face 'custom-face-tag                        'nano-face-strong)
    (set-face 'custom-variable-tag                    'nano-face-strong)
    (set-face 'custom-invalid                         'nano-face-popout)
    (set-face 'custom-visibility                     'nano-face-salient)
    (set-face 'custom-state                          'nano-face-salient)
    (set-face 'custom-link                           'nano-face-salient)))

(defun nano-theme--package ()
  "Derive package faces from nano faces."
  (with-eval-after-load 'package
    (set-face 'package-description                   'nano-face-default)
    (set-face 'package-help-section-name             'nano-face-default)
    (set-face 'package-name                          'nano-face-salient)
    (set-face 'package-status-avail-obso               'nano-face-faded)
    (set-face 'package-status-available              'nano-face-default)
    (set-face 'package-status-built-in               'nano-face-salient)
    (set-face 'package-status-dependency             'nano-face-salient)
    (set-face 'package-status-disabled                 'nano-face-faded)
    (set-face 'package-status-external               'nano-face-default)
    (set-face 'package-status-held                   'nano-face-default)
    (set-face 'package-status-incompat                 'nano-face-faded)
    (set-face 'package-status-installed              'nano-face-salient)
    (set-face 'package-status-new                    'nano-face-default)
    (set-face 'package-status-unsigned               'nano-face-default))

  ;; Button face is hardcoded, we have to redefine the relevant
  ;; function
  (defun package-make-button (text &rest properties)
    "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
    (let ((button-text (if (display-graphic-p)
                           text (concat "[" text "]")))
          (button-face (if (display-graphic-p)
                           `(:box `(:line-width 1
                                    :color ,nano-color-subtle
                                    :style nil)
                                  :foreground ,nano-color-faded
                                  :background ,nano-color-subtle)
                         'link)))
      (apply #'insert-text-button button-text
               'face button-face 'follow-link t properties))))


(defun nano-theme--flyspell ()
  "Derive flyspell faces from nano faces."
  (with-eval-after-load 'flyspell
    (set-face 'flyspell-duplicate                     'nano-face-popout)
    (set-face 'flyspell-incorrect                     'nano-face-popout)))


(defun nano-theme--ido ()
  "Derive ido faces from nano faces."
  (with-eval-after-load 'ido
    (set-face 'ido-first-match                       'nano-face-salient)
    (set-face 'ido-only-match                          'nano-face-faded)
    (set-face 'ido-subdir                             'nano-face-strong)))


(defun nano-theme--diff ()
  "Derive diff faces from nano faces."
  (with-eval-after-load 'diff-mode
    (set-face 'diff-header                             'nano-face-faded)
    (set-face 'diff-file-header                       'nano-face-strong)
    (set-face 'diff-context                          'nano-face-default)
    (set-face 'diff-removed                            'nano-face-faded)
    (set-face 'diff-changed                           'nano-face-popout)
    (set-face 'diff-added                            'nano-face-salient)
    (set-face 'diff-refine-added                    '(nano-face-salient
                                                      nano-face-strong))
    (set-face 'diff-refine-changed                    'nano-face-popout)
    (set-face 'diff-refine-removed                    'nano-face-faded)
    (set-face-attribute     'diff-refine-removed nil :strike-through t)))


(defun nano-theme--term ()
  "Derive term faces from nano faces, and material theme colors."
  (with-eval-after-load 'term
    ;; (setq eterm-256color-disable-bold nil)
    (set-face 'term-bold                                   'nano-face-strong)
    (set-face-attribute 'term-color-black nil
                         :foreground (face-foreground 'nano-face-default)
                         :background (face-foreground 'nano-face-default))
    (set-face-attribute 'term-color-white nil
                         :foreground (face-background 'nano-face-default)
                         :background (face-background 'nano-face-default))
    (set-face-attribute 'term-color-blue nil
                         :foreground "#42A5F5"   ;; material color blue L400
                         :background "#BBDEFB")  ;; material color blue L100
    (set-face-attribute 'term-color-cyan nil
                         :foreground "#26C6DA"   ;; material color cyan L400
                         :background "#B2EBF2")  ;; material color cyan L100
    (set-face-attribute 'term-color-green nil
                         :foreground "#66BB6A"   ;; material color green L400
                         :background "#C8E6C9")  ;; material color green L100
    (set-face-attribute 'term-color-magenta nil
                         :foreground "#AB47BC"   ;; material color purple L400
                         :background "#E1BEE7")  ;; material color purple L100
    (set-face-attribute 'term-color-red nil
                         :foreground "#EF5350"   ;; material color red L400
                         :background "#FFCDD2")  ;; material color red L100
    (set-face-attribute 'term-color-yellow nil
                         :foreground "#FFEE58"    ;; material color yellow L400
                         :background "#FFF9C4"))) ;; material color yellow L100


(defun nano-theme--calendar ()
  "Derive calendar faces from nano faces."
  (with-eval-after-load 'calendar
    (set-face 'calendar-today                         'nano-face-strong)))


(defun nano-theme--agenda ()
  "Derive agenda faces from nano faces."
  (with-eval-after-load 'org-agenda
    (set-face 'org-agenda-calendar-event             'nano-face-default)
    (set-face 'org-agenda-calendar-sexp              'nano-face-salient)
    (set-face 'org-agenda-clocking                     'nano-face-faded)
    (set-face 'org-agenda-column-dateline              'nano-face-faded)
    (set-face 'org-agenda-current-time                'nano-face-strong)
    (set-face 'org-agenda-date                       'nano-face-salient)
    (set-face 'org-agenda-date-today                '(nano-face-salient
                                                       nano-face-strong))
    (set-face 'org-agenda-date-weekend                 'nano-face-faded)
    (set-face 'org-agenda-diary                        'nano-face-faded)
    (set-face 'org-agenda-dimmed-todo-face             'nano-face-faded)
    (set-face 'org-agenda-done                         'nano-face-faded)
    (set-face 'org-agenda-filter-category              'nano-face-faded)
    (set-face 'org-agenda-filter-effort                'nano-face-faded)
    (set-face 'org-agenda-filter-regexp                'nano-face-faded)
    (set-face 'org-agenda-filter-tags                  'nano-face-faded)
    ;;  (set-face 'org-agenda-property-face                'nano-face-faded)
    (set-face 'org-agenda-restriction-lock             'nano-face-faded)
    (set-face 'org-agenda-structure                   'nano-face-strong)))


(defun nano-theme--org ()
  "Derive org faces from nano faces."
  (with-eval-after-load 'org
    (set-face 'org-archived                            'nano-face-faded)

    (set-face 'org-block                                       'hl-line)
    (set-face 'org-block-begin-line                    'nano-face-faded)
    (set-face 'org-block-end-line                      'nano-face-faded)
    (unless (version< emacs-version "27.0")
      (set-face-attribute 'org-block nil                      :extend t)
      (set-face-attribute 'org-block-begin-line nil           :extend t)
      (set-face-attribute 'org-block-end-line nil             :extend t))

    (set-face 'org-checkbox                            'nano-face-faded)
    (set-face 'org-checkbox-statistics-done            'nano-face-faded)
    (set-face 'org-checkbox-statistics-todo            'nano-face-faded)
    (set-face 'org-clock-overlay                       'nano-face-faded)
    (set-face 'org-code                                'nano-face-faded)
    (set-face 'org-column                              'nano-face-faded)
    (set-face 'org-column-title                        'nano-face-faded)
    (set-face 'org-date                                'nano-face-faded)
    (set-face 'org-date-selected                       'nano-face-faded)
    (set-face 'org-default                             'nano-face-faded)
    (set-face 'org-document-info                       'nano-face-faded)
    (set-face 'org-document-info-keyword               'nano-face-faded)
    (set-face 'org-document-title                      'nano-face-faded)
    (set-face 'org-done                              'nano-face-default)
    (set-face 'org-drawer                              'nano-face-faded)
    (set-face 'org-ellipsis                            'nano-face-faded)
    (set-face 'org-footnote                            'nano-face-faded)
    (set-face 'org-formula                             'nano-face-faded)
    (set-face 'org-headline-done                       'nano-face-faded)
    ;; (set-face 'org-hide                             'nano-face-faded)
    ;; (set-face 'org-indent                           'nano-face-faded)
    (set-face 'org-latex-and-related                   'nano-face-faded)
    (set-face 'org-level-1                            'nano-face-strong)
    (set-face 'org-level-2                            'nano-face-strong)
    (set-face 'org-level-3                            'nano-face-strong)
    (set-face 'org-level-4                            'nano-face-strong)
    (set-face 'org-level-5                            'nano-face-strong)
    (set-face 'org-level-6                            'nano-face-strong)
    (set-face 'org-level-7                            'nano-face-strong)
    (set-face 'org-level-8                            'nano-face-strong)
    (set-face 'org-link                              'nano-face-salient)
    (set-face 'org-list-dt                             'nano-face-faded)
    (set-face 'org-macro                               'nano-face-faded)
    (set-face 'org-meta-line                           'nano-face-faded)
    (set-face 'org-mode-line-clock                     'nano-face-faded)
    (set-face 'org-mode-line-clock-overrun             'nano-face-faded)
    (set-face 'org-priority                            'nano-face-faded)
    (set-face 'org-property-value                      'nano-face-faded)
    (set-face 'org-quote                               'nano-face-faded)
    (set-face 'org-scheduled                           'nano-face-faded)
    (set-face 'org-scheduled-previously                'nano-face-faded)
    (set-face 'org-scheduled-today                     'nano-face-faded)
    (set-face 'org-sexp-date                           'nano-face-faded)
    (set-face 'org-special-keyword                     'nano-face-faded)
    (set-face 'org-table                               'nano-face-faded)
    (set-face 'org-tag                                'nano-face-popout)
    (set-face 'org-tag-group                           'nano-face-faded)
    (set-face 'org-target                              'nano-face-faded)
    (set-face 'org-time-grid                           'nano-face-faded)
    (set-face 'org-todo                              'nano-face-salient)
    (set-face 'org-upcoming-deadline                 'nano-face-default)
    (set-face 'org-verbatim                           'nano-face-popout)
    (set-face 'org-verse                               'nano-face-faded)
    (set-face 'org-warning                            'nano-face-popout)))


(defun nano-theme--mu4e ()
  "Derive mu4e faces from nano faces."
  (with-eval-after-load 'mu4e
    (set-face 'mu4e-attach-number-face                'nano-face-strong)
    (set-face 'mu4e-cited-1-face                       'nano-face-faded)
    (set-face 'mu4e-cited-2-face                       'nano-face-faded)
    (set-face 'mu4e-cited-3-face                       'nano-face-faded)
    (set-face 'mu4e-cited-4-face                       'nano-face-faded)
    (set-face 'mu4e-cited-5-face                       'nano-face-faded)
    (set-face 'mu4e-cited-6-face                       'nano-face-faded)
    (set-face 'mu4e-cited-7-face                       'nano-face-faded)
    (set-face 'mu4e-compose-header-face                'nano-face-faded)
    (set-face 'mu4e-compose-separator-face             'nano-face-faded)
    (set-face 'mu4e-contact-face                     'nano-face-salient)
    (set-face 'mu4e-context-face                       'nano-face-faded)
    (set-face 'mu4e-draft-face                         'nano-face-faded)
    (set-face 'mu4e-flagged-face                      'nano-face-popout)
    (set-face 'mu4e-footer-face                        'nano-face-faded)
    (set-face 'mu4e-forwarded-face                   'nano-face-default)
    (set-face 'mu4e-header-face                      'nano-face-default)
    (set-face 'mu4e-header-highlight-face                      'hl-line)
    (set-face 'mu4e-header-key-face                   'nano-face-strong)
    (set-face 'mu4e-header-marks-face                  'nano-face-faded)
    (set-face 'mu4e-header-title-face                 'nano-face-strong)
    (set-face 'mu4e-header-value-face                'nano-face-default)
    (set-face 'mu4e-highlight-face                    'nano-face-popout)
    (set-face 'mu4e-link-face                        'nano-face-salient)
    (set-face 'mu4e-modeline-face                      'nano-face-faded)
    (set-face 'mu4e-moved-face                         'nano-face-faded)
    (set-face 'mu4e-ok-face                            'nano-face-faded)
    (set-face 'mu4e-region-code                        'nano-face-faded)
    (set-face 'mu4e-replied-face                     'nano-face-default)
    (set-face 'mu4e-special-header-value-face        'nano-face-default)
    (set-face 'mu4e-system-face                        'nano-face-faded)
    (set-face 'mu4e-title-face                        'nano-face-strong)
    (set-face 'mu4e-trashed-face                       'nano-face-faded)
    (set-face 'mu4e-unread-face                       'nano-face-strong)
    ;;(set-face-attribute 'mu4e-unread-face nil :weight 'regular)
    (set-face 'mu4e-url-number-face                    'nano-face-faded)
    (set-face 'mu4e-view-body-face                   'nano-face-default)
    (set-face 'mu4e-warning-face                      'nano-face-popout)))


(defun nano-theme--elfeed ()
  "Derive elfeed faces from nano faces."
  (with-eval-after-load 'elfeed
    (set-face 'elfeed-log-date-face                    'nano-face-faded)
    (set-face 'elfeed-log-info-level-face            'nano-face-default)
    (set-face 'elfeed-log-debug-level-face           'nano-face-default)
    (set-face 'elfeed-log-warn-level-face             'nano-face-popout)
    (set-face 'elfeed-log-error-level-face            'nano-face-popout)
    (set-face 'elfeed-search-tag-face                  'nano-face-faded)
    (set-face 'elfeed-search-date-face                 'nano-face-faded)
    (set-face 'elfeed-search-feed-face               'nano-face-salient)
    (set-face 'elfeed-search-filter-face               'nano-face-faded)
    (set-face 'elfeed-search-last-update-face        'nano-face-salient)
    (set-face 'elfeed-search-title-face              'nano-face-default)
    (set-face 'elfeed-search-tag-face                  'nano-face-faded)
    (set-face 'elfeed-search-unread-count-face        'nano-face-strong)
    (set-face 'elfeed-search-unread-title-face        'nano-face-strong)))

(defun nano-theme--deft ()
  "Derive deft faces from nano faces."
  (with-eval-after-load 'deft
    (set-face 'deft-filter-string-error-face         'nano-face-popout)
    (set-face 'deft-filter-string-face              'nano-face-default)
    (set-face 'deft-header-face                     'nano-face-salient)
    (set-face 'deft-separator-face                    'nano-face-faded)
    (set-face 'deft-summary-face                      'nano-face-faded)
    (set-face 'deft-time-face                       'nano-face-salient)
    (set-face 'deft-title-face                       'nano-face-strong)))

(defun nano-theme--rst ()
  "Derive rst faces from nano faces."
  (with-eval-after-load 'rst
    (set-face 'rst-adornment                           'nano-face-faded)
    (set-face 'rst-block                             'nano-face-default)
    (set-face 'rst-comment                             'nano-face-faded)
    (set-face 'rst-definition                        'nano-face-salient)
    (set-face 'rst-directive                         'nano-face-salient)
    (set-face 'rst-emphasis1                           'nano-face-faded)
    (set-face 'rst-emphasis2                          'nano-face-strong)
    (set-face 'rst-external                          'nano-face-salient)
    (set-face 'rst-level-1                            'nano-face-strong)
    (set-face 'rst-level-2                            'nano-face-strong)
    (set-face 'rst-level-3                            'nano-face-strong)
    (set-face 'rst-level-4                            'nano-face-strong)
    (set-face 'rst-level-5                            'nano-face-strong)
    (set-face 'rst-level-6                            'nano-face-strong)
    (set-face 'rst-literal                           'nano-face-salient)
    (set-face 'rst-reference                         'nano-face-salient)
    (set-face 'rst-transition                        'nano-face-default)))


(defun nano-theme--markdown ()
  "Derive markdown faces from nano faces."
  (with-eval-after-load 'markdown-mode
    (set-face 'markdown-blockquote-face              'nano-face-default)
    (set-face 'markdown-bold-face                     'nano-face-strong)
    (set-face 'markdown-code-face                    'nano-face-default)
    (set-face 'markdown-comment-face                   'nano-face-faded)
    (set-face 'markdown-footnote-marker-face         'nano-face-default)
    (set-face 'markdown-footnote-text-face           'nano-face-default)
    (set-face 'markdown-gfm-checkbox-face            'nano-face-default)
    (set-face 'markdown-header-delimiter-face          'nano-face-faded)
    (set-face 'markdown-header-face                   'nano-face-strong)
    (set-face 'markdown-header-face-1                 'nano-face-strong)
    (set-face 'markdown-header-face-2                 'nano-face-strong)
    (set-face 'markdown-header-face-3                 'nano-face-strong)
    (set-face 'markdown-header-face-4                 'nano-face-strong)
    (set-face 'markdown-header-face-5                 'nano-face-strong)
    (set-face 'markdown-header-face-6                'nano-face-strong)
    (set-face 'markdown-header-rule-face             'nano-face-default)
    (set-face 'markdown-highlight-face               'nano-face-default)
    (set-face 'markdown-hr-face                      'nano-face-default)
    (set-face 'markdown-html-attr-name-face          'nano-face-default)
    (set-face 'markdown-html-attr-value-face         'nano-face-default)
    (set-face 'markdown-html-entity-face             'nano-face-default)
    (set-face 'markdown-html-tag-delimiter-face      'nano-face-default)
    (set-face 'markdown-html-tag-name-face           'nano-face-default)
    (set-face 'markdown-inline-code-face              'nano-face-popout)
    (set-face 'markdown-italic-face                    'nano-face-faded)
    (set-face 'markdown-language-info-face           'nano-face-default)
    (set-face 'markdown-language-keyword-face        'nano-face-default)
    (set-face 'markdown-line-break-face              'nano-face-default)
    (set-face 'markdown-link-face                    'nano-face-salient)
    (set-face 'markdown-link-title-face              'nano-face-default)
    (set-face 'markdown-list-face                      'nano-face-faded)
    (set-face 'markdown-markup-face                    'nano-face-faded)
    (set-face 'markdown-math-face                    'nano-face-default)
    (set-face 'markdown-metadata-key-face              'nano-face-faded)
    (set-face 'markdown-metadata-value-face            'nano-face-faded)
    (set-face 'markdown-missing-link-face            'nano-face-default)
    (set-face 'markdown-plain-url-face               'nano-face-default)
    (set-face 'markdown-pre-face                     'nano-face-default)
    (set-face 'markdown-reference-face               'nano-face-salient)
    (set-face 'markdown-strike-through-face            'nano-face-faded)
    (set-face 'markdown-table-face                   'nano-face-default)
    (set-face 'markdown-url-face                     'nano-face-salient)))


(defun nano-theme--ivy ()
  "Derive ivy faces from nano faces."
  (with-eval-after-load 'ivy
    (set-face 'ivy-action                              'nano-face-faded)
    (set-face 'ivy-completions-annotations             'nano-face-faded)
    (set-face 'ivy-confirm-face                        'nano-face-faded)
    (set-face 'ivy-current-match    '(nano-face-strong nano-face-subtle))
    (set-face 'ivy-cursor                             'nano-face-strong)
    (set-face 'ivy-grep-info                          'nano-face-strong)
    (set-face 'ivy-grep-line-number                    'nano-face-faded)
    (set-face 'ivy-highlight-face                     'nano-face-strong)
    (set-face 'ivy-match-required-face                 'nano-face-faded)
    (set-face 'ivy-minibuffer-match-face-1             'nano-face-faded)
    (set-face 'ivy-minibuffer-match-face-2             'nano-face-faded)
    (set-face 'ivy-minibuffer-match-face-3             'nano-face-faded)
    (set-face 'ivy-minibuffer-match-face-4             'nano-face-faded)
    (set-face 'ivy-minibuffer-match-highlight         'nano-face-strong)
    (set-face 'ivy-modified-buffer                    'nano-face-popout)
    (set-face 'ivy-modified-outside-buffer            'nano-face-strong)
    (set-face 'ivy-org                                 'nano-face-faded)
    (set-face 'ivy-prompt-match                        'nano-face-faded)
    (set-face 'ivy-remote                            'nano-face-default)
    (set-face 'ivy-separator                           'nano-face-faded)
    (set-face 'ivy-subdir                              'nano-face-faded)
    (set-face 'ivy-virtual                             'nano-face-faded)
    (set-face 'ivy-yanked-word                         'nano-face-faded)))

(defun nano-theme--helm ()
  "Derive helm faces from nano faces."
  (with-eval-after-load 'helm
    (set-face 'helm-selection                '(nano-face-strong nano-face-subtle))
    (set-face 'helm-match                                       'nano-face-strong)
    (set-face 'helm-source-header                              'nano-face-salient)
    (set-face 'helm-visible-mark                                'nano-face-strong)))

(defun nano-theme--helm-swoop ()
  "Derive helm faces from nano faces."
  (with-eval-after-load 'helm-swoop
    (set-face 'helm-swoop-target-line-face   '(nano-face-strong nano-face-subtle))))

(defun nano-theme--helm-occur ()
  "Derive helm faces from nano faces."
  (with-eval-after-load 'helm-occur
    (set-face 'helm-moccur-buffer                               'nano-face-strong)))

(defun nano-theme--helm-ff ()
  "Derive helm faces from nano faces."
  (with-eval-after-load 'helm-ff
    (set-face 'helm-ff-file                                      'nano-face-faded)
    (set-face 'helm-ff-prefix                                   'nano-face-strong)
    (set-face 'helm-ff-dotted-directory                          'nano-face-faded)
    (set-face 'helm-ff-directory                                'nano-face-strong)
    (set-face 'helm-ff-executable                               'nano-face-popout)))

(defun nano-theme--helm-grep ()
  "Derive helm faces from nano faces."
  (with-eval-after-load 'helm-grep
    (set-face 'helm-grep-match                                  'nano-face-strong)
    (set-face 'helm-grep-file                                    'nano-face-faded)
    (set-face 'helm-grep-lineno                                  'nano-face-faded)
    (set-face 'helm-grep-finish                                'nano-face-default)))

(defun nano-theme--company ()
  "Derive company tooltip window from nano faces."
  (with-eval-after-load 'company
    (set-face 'company-tooltip-selection                   '(nano-face-strong nano-face-subtle))
    (set-face-attribute 'company-tooltip-selection nil :background nano-color-popout)
    
    (set-face 'company-tooltip                                               'nano-face-subtle)

    (set-face 'company-scrollbar-fg                                          'nano-face-faded)
    (set-face-attribute 'company-scrollbar-fg nil :background nano-color-foreground)
    
    (set-face 'company-scrollbar-bg                                          'nano-face-default)
    (set-face-attribute 'company-scrollbar-bg nil :background nano-color-faded)

    (set-face 'company-tooltip-common                                        'nano-face-faded)
    (set-face 'company-tooltip-common-selection            '(nano-face-strong nano-face-subtle))
    (set-face-attribute 'company-tooltip-common-selection nil :background nano-color-popout)
    
    (set-face 'company-tooltip-annotation                                    'nano-face-default)
    (set-face 'company-tooltip-annotation-selection        '(nano-face-strong nano-face-subtle))))

(defun nano-theme ()
  "Derive many, many faces from the core nano faces."
  (nano-theme--basics)
  (nano-theme--font-lock)
  (nano-theme--mode-line)
  (nano-theme--minibuffer)
  (nano-theme--buttons)
  (nano-theme--info)
  (nano-theme--bookmark)
  (nano-theme--speedbar)
  (nano-theme--message)
  (nano-theme--outline)
  (nano-theme--customize)
  (nano-theme--package)
  (nano-theme--flyspell)
  (nano-theme--ido)
  (nano-theme--diff)
  (nano-theme--term)
  (nano-theme--calendar)
  (nano-theme--agenda)
  (nano-theme--org)
  (nano-theme--mu4e)
  (nano-theme--elfeed)
  (nano-theme--deft)
  (nano-theme--rst)
  (nano-theme--markdown)
  (nano-theme--ivy)
  (nano-theme--helm)
  (nano-theme--helm-swoop)
  (nano-theme--helm-occur)
  (nano-theme--helm-ff)
  (nano-theme--helm-grep)
  (nano-theme--hl-line)
  (nano-theme--company))

(provide 'nano-theme)
