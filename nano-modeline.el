;; -------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - Nicolas .P Rougier
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
;;
;; Nano mode line format:
;;
;; [ status | name | primary                               secondary ]
;;
;; -------------------------------------------------------------------
(require 'subr-x)
;; (require 'all-the-icons)

;; ---------------------------------------------------------------------
(defun vc-branch ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat "#" (substring-no-properties vc-mode
                                 (+ (if (eq backend 'Hg) 2 3) 2))))  nil))


;; From https://amitp.blogspot.com/2011/08/emacs-custom-mode-line.html
;; ---------------------------------------------------------------------
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat "…/" output)))
    output))

;; ---------------------------------------------------------------------
(defun nano-modeline-compose (status name primary secondary &optional pad)
  "Compose a string with provided information"
  (let* ((status-face-rw `(:foreground ,(face-background 'nano-face-default)
                           :background ,(face-foreground 'nano-face-faded)))
         (status-face-ro `(:foreground ,(face-background 'nano-face-default)
                           :background ,(face-foreground 'nano-face-popout)))
         (status-face-** `(:foreground ,(face-background 'nano-face-default)
                           :background ,(face-background 'nano-face-critical)))
         (pad            (or pad 1))
         (space-up       +0.15)
         (space-down     -0.20)
         (left (concat
                (cond ((string= status "RO")
                       (propertize " RO " 'face status-face-ro))
                      ((string= status "**")
                       (propertize " ** " 'face status-face-**))
                      ((string= status "RW")
                       (propertize " RW " 'face status-face-rw))
                      (t (propertize status 'face status-face-ro))
                      )
                (propertize " " 'display `(raise ,space-up))
                (propertize name 'face 'nano-face-strong)
                (propertize " " 'display `(raise ,space-down)) primary))
         (right secondary)
;;         (available-width (- (window-total-width nil 'floor) (length left) 1)))
         (available-width (- (window-body-width) (length left) pad)))
    (format (format "%%s%%%ds" available-width) left right)))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-dashboard-mode-p ()
  (bound-and-true-p mu4e-dashboard-mode))
  
(defun nano-modeline-mu4e-dashboard-mode ()
  (nano-modeline-compose (nano-modeline-status)
			 ;; (nano-modeline-status-icon "envelope")
                         "Mail"
                         (nano-modeline-mu4e-context)
                         ""))

;; ---------------------------------------------------------------------
(defun nano-modeline-elfeed-search-mode-p ()
  (derived-mode-p 'elfeed-search-mode))

(defun nano-modeline-elfeed-search-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         ;; (nano-modeline-status-icon "newspaper-o")
                         "Elfeed"
                         (concat "(" (elfeed-search--header)  ")")
                         ""))

;; Elfeed (regular header)
(with-eval-after-load 'elfeed
  (defun elfeed-setup-header ()
    (setq header-line-format (default-value 'header-line-format)))
  (setq elfeed-search-header-function #'elfeed-setup-header))

;; ---------------------------------------------------------------------
(defun nano-modeline-elfeed-show-mode-p ()
  (derived-mode-p 'elfeed-show-mode))

(defun nano-modeline-elfeed-show-mode ()
  (let* ((title        (elfeed-entry-title elfeed-show-entry))
         (tags         (elfeed-entry-tags elfeed-show-entry))
         (tags-str     (mapconcat #'symbol-name tags ", "))
         (date         (seconds-to-time (elfeed-entry-date elfeed-show-entry)))
         (feed         (elfeed-entry-feed elfeed-show-entry))
         (feed-title   (plist-get (elfeed-feed-meta feed) :title))
         (entry-author (elfeed-meta elfeed-show-entry :author)))
    (nano-modeline-compose status
			   ;; (nano-modeline-status-icon "newspaper-o")
                           (s-truncate 40 title "…")
                           (concat "(" tags-str ")")
                           feed-title)))

;; ---------------------------------------------------------------------
(defun nano-modeline-calendar-mode-p ()
  (derived-mode-p 'calendar-mode))

(defun nano-modeline-calendar-mode () "")

;; Calendar (no header, only overline)
(with-eval-after-load 'calendar
  (defun calendar-setup-header ()
    (setq header-line-format "")
    (face-remap-add-relative
     'header-line `(:overline ,(face-foreground 'default)
                    :height 0.5
                    :background ,(face-background 'default))))
  (add-hook 'calendar-initial-window-hook #'calendar-setup-header)

  ;; From https://emacs.stackexchange.com/questions/45650
  (add-to-list 'display-buffer-alist
               `(,(rx string-start "*Calendar*" string-end)
                 (display-buffer-below-selected))))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-capture-mode-p ()
  (bound-and-true-p org-capture-mode))

(defun nano-modeline-org-capture-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Capture"
                         "(org)"
                         "[C-c C-c] Finish [C-c C-w] Refile [C-c C-k] Abort"))

(with-eval-after-load 'org-capture
  (defun org-capture-turn-off-header-line ()
    (setq-local header-line-format (default-value 'header-line-format))
    ;; (fit-window-to-buffer nil nil 8)
    ;; (face-remap-add-relative 'header-line '(:background "#ffffff"))
    (message nil))
  (add-hook 'org-capture-mode-hook
            #'org-capture-turn-off-header-line))

;; ---------------------------------------------------------------------
(defun nano-modeline-org-agenda-mode-p ()
  (derived-mode-p 'org-agenda-mode))

(defun nano-modeline-org-agenda-mode ()
  (nano-modeline-compose (nano-modeline-status)
			 ;; (nano-modeline-status-icon "calendar")
                         "Agenda"
                         ""
                         (format-time-string "%H:%M")
                         ))

;; ---------------------------------------------------------------------
(defun nano-modeline-term-mode-p ()
  (derived-mode-p 'term-mode))

(defun nano-modeline-term-mode ()
  (nano-modeline-compose " >_ "
			 ;; (nano-modeline-status-icon "terminal")
                         "Terminal"
                         (concat "(" shell-file-name ")")
                         (shorten-directory default-directory 32)))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-main-mode-p ()
  (derived-mode-p 'mu4e-main-mode))

(defun nano-modeline-mu4e-main-mode ()
  (nano-modeline-compose (nano-modeline-status)
                         "Mail"
                         (nano-modeline-mu4e-context)
                         (format-time-string "%A %d %B %Y, %H:%M")))

;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-headers-mode-p ()
  (derived-mode-p 'mu4e-headers-mode))

(defun nano-modeline-mu4e-headers-mode ()
  (nano-modeline-compose (nano-modeline-status)
			 ;;(nano-modeline-status-icon "envelope-o")
                         (mu4e~quote-for-modeline mu4e~headers-last-query)
                         ""
                         ""))

(with-eval-after-load 'mu4e
  (defun mu4e~header-line-format () (nano-modeline)))

;; ---------------------------------------------------------------------
(setq mu4e-modeline-max-width 72)

(defun nano-modeline-mu4e-view-mode-p ()
  (derived-mode-p 'mu4e-view-mode))

(defun nano-modeline-mu4e-view-mode ()
  (let* ((msg     (mu4e-message-at-point))
         (subject (mu4e-message-field msg :subject))
         (from    (mu4e~headers-contact-str (mu4e-message-field msg :from)))
         (date     (mu4e-message-field msg :date)))
    (nano-modeline-compose (nano-modeline-status)
                           (s-truncate 40 subject "…")
                           ""
                           from)))

(add-hook 'mu4e-view-mode-hook
          (lambda () (setq header-line-format "%-")
                     (face-remap-add-relative 'header-line
                                              '(:background "#ffffff" :height 1.0))))

;; ---------------------------------------------------------------------
(setq org-mode-line-string nil)
(with-eval-after-load 'org-clock
  (add-hook 'org-clock-out-hook
            '(lambda () (setq org-mode-line-string nil)
                        (force-mode-line-update))))

(defun nano-modeline-org-clock-mode-p ()
  org-mode-line-string)

(defun nano-modeline-org-clock-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (format-mode-line "%m"))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name 
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                             (propertize branch 'face 'italic)))
                                     ")" )
                             org-mode-line-string)))

;; ---------------------------------------------------------------------
(defun nano-modeline-default-mode ()
    (let ((buffer-name (format-mode-line "%b"))
          (mode-name   (format-mode-line "%m"))
          (branch      (vc-branch))
          (position    (format-mode-line "%l:%c")))
      (nano-modeline-compose (nano-modeline-status)
                             buffer-name
                             (concat "(" mode-name
                                     (if branch (concat ", "
                                            (propertize branch 'face 'italic)))
                                     ")" )
                             position)))

;; ---------------------------------------------------------------------
(defun nano-modeline-status-icon (name &optional fmt fg-color bg-color)
  "Return a propertized string containing icon NAME from the font
   awesome family using the provided FMT to format the string (%s
   is replaced with icon, default is ' %s  '). Foreground (FG-COLOR)
   and background (BG-COLOR) can be specified. If not, foreground
   color is the default background (white) and background is the
   foreground of the 'nano-face-popout face."

  (let ((fg-color (or fg-color (face-background 'nano-face-default)))
        (bg-color (or bg-color (face-foreground 'nano-face-popout)))
        (fmt      (or fmt "  %s  ")))
         
    (propertize (format fmt (all-the-icons-faicon name))
                'face `(:family ,(all-the-icons-faicon-family)
                                :height 1.1
                                :foreground ,fg-color
                                :background ,bg-color)
                'display '(raise 0.0))))

;; ---------------------------------------------------------------------
(defun nano-modeline-status ()
  "Return buffer status: read-only (RO), modified (**) or read-write (RW)"
  
  (let ((read-only   buffer-read-only)
        (modified    (and buffer-file-name (buffer-modified-p))))
    (cond (modified  "**") (read-only "RO") (t "RW"))))
  
;; ---------------------------------------------------------------------
(defun nano-modeline-mu4e-context ()
  "Return the current mu4e context as a non propertized string."

  (if (> (length (mu4e-context-label)) 0)
      (concat "(" (substring-no-properties (mu4e-context-label) 1 -1) ")")
    "(none)"))


;; ---------------------------------------------------------------------
(defun nano-modeline ()
  "Install a header line whose content is dependend on the major mode"
  (interactive)
  (setq-default header-line-format
  '((:eval
     (cond ((nano-modeline-elfeed-search-mode-p)   (nano-modeline-elfeed-search-mode))
           ((nano-modeline-elfeed-show-mode-p)     (nano-modeline-elfeed-show-mode))
           ((nano-modeline-calendar-mode-p)        (nano-modeline-calendar-mode))
           ((nano-modeline-org-capture-mode-p)     (nano-modeline-org-capture-mode))
           ((nano-modeline-org-agenda-mode-p)      (nano-modeline-org-agenda-mode))
           ((nano-modeline-org-clock-mode-p)       (nano-modeline-org-clock-mode))
           ((nano-modeline-term-mode-p)            (nano-modeline-term-mode))
           ((nano-modeline-mu4e-dashboard-mode-p)  (nano-modeline-mu4e-dashboard-mode))
           ((nano-modeline-mu4e-main-mode-p)       (nano-modeline-mu4e-main-mode))
           ((nano-modeline-mu4e-headers-mode-p)    (nano-modeline-mu4e-headers-mode))
;;           ((nano-modeline-mu4e-view-mode-p)       (nano-modeline-mu4e-view-mode))
           (t                                      (nano-modeline-default-mode)))))))

;; ---------------------------------------------------------------------
(defun nano-modeline-update-windows ()
  "Modify the mode line depending on the presence of a window below."
  
  (dolist (window (window-list))
    (with-selected-window window
      (if (or (one-window-p t)
	      (eq (window-in-direction 'below) (minibuffer-window))
	      (not (window-in-direction 'below)))
	      (with-current-buffer (window-buffer window)
	        (setq mode-line-format "%-"))
	    (with-current-buffer (window-buffer window)
 	      (setq mode-line-format nil)))
;;      (if (window-in-direction 'above)
;;	      (face-remap-add-relative 'header-line '(:overline "#777777"))
;;	    (face-remap-add-relative 'header-line '(:overline nil)))
      )))
(add-hook 'window-configuration-change-hook 'nano-modeline-update-windows)

(setq-default mode-line-format "%-")
(nano-modeline)

(provide 'nano-modeline)
