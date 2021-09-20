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
;; An experimental interactive nano-agenda that displays side by side a
;; mini calendar on the left and timestamped org entries on the right.
;; ---------------------------------------------------------------------
(require 'ts)
(require 'org)
(require 'org-agenda)
(require 'calendar)
(require 'holidays)


;; --- Faces -----------------------------------------------------------
(defgroup nano-agenda-faces nil
  "Nano-Agenda faces")

(defface nano-agenda-face-default
  '((t :inherit 'default ))
  "Default face"
  :group 'nano-agenda-faces)

(defface nano-agenda-face-selected
  `((t :foreground ,nano-light-background
       :background ,nano-light-foreground ))
  "Face for the selected day"
  :group 'nano-agenda-faces)

(defface nano-agenda-face-today
  `((t :foreground ,nano-light-popout
       :inherit 'bold ))
  "Today face when not selected."
  :group 'nano-agenda-faces)

(defface nano-agenda-face-selected-today
  `((t :foreground ,nano-light-background
       :background ,nano-light-popout ))
  "Today face when selected."
  :group 'nano-agenda-faces)

(defface nano-agenda-face-weekend
  `((t :foreground ,nano-light-faded ))
  "Weekend face"
  :group 'nano-agenda-faces)

(defface nano-agenda-face-holidays
  `((t :foreground ,nano-light-faded ))
  "Holidays face"
  :group 'nano-agenda-faces)

(defface nano-agenda-face-outday
  `((t :foreground ,nano-light-subtle ))
  "Out day face"
  :group 'nano-agenda-faces)

(defface nano-agenda-face-day-name
  `((t :foreground ,nano-light-faded ))
  "Day name face (on second line)"
  :group 'nano-agenda-faces)

(defface nano-agenda-face-month-name
  '((t :inherit 'bold ))
  "Month name face (on first line)"
  :group 'nano-agenda-faces)

(defface nano-agenda-face-mouse
  '((t :inherit 'highlight ))
  "Mouse highlight face"
  :group 'nano-agenda-faces)

(defface nano-agenda-face-button
  `((t :foreground ,nano-light-faded ))
  "Header button (left and right)"
  :group 'nano-agenda-faces)


;; --- Global variable -------------------------------------------------
(setq nano-agenda-selected (ts-now))
(setq nano-agenda-file "~/Documents/org/agenda.org")


;; --- Useful functions ------------------------------------------------
(defun center-string (string size)
  (let* ((padding (/ (- size (length string)) 2))
         (lpad (+ (length string) padding))
         (lformat (format "%%%ds" lpad))
         (rformat (format "%%%ds" (- size))))
    (format rformat (format lformat string))))


;; --- Nano-Agenda minor mode ---------------------------------------------
(define-minor-mode nano-agenda-mode
  "Minor mode for nano-agenda."
  :init nil
  :lighter "Calendar"
  :keymap (make-sparse-keymap)
  
  (when nano-agenda-mode
    (setq buffer-read-only t)
    (setq cursor-type nil)
    (local-set-key (kbd "<left>") #'nano-agenda-backward-day)
    (local-set-key (kbd "<right>") #'nano-agenda-forward-day)
    (local-set-key (kbd "<up>") #'nano-agenda-backward-week)
    (local-set-key (kbd "<down>") #'nano-agenda-forward-week)
    (local-set-key (kbd "<S-left>") #'nano-agenda-backward-month)
    (local-set-key (kbd "<S-right>") #'nano-agenda-forward-month)
    (local-set-key (kbd "<S-up>") #'nano-agenda-backward-year)
    (local-set-key (kbd "<S-down>") #'nano-agenda-forward-year)
    (local-set-key (kbd ".") #'nano-agenda-select-today)
    (local-set-key (kbd "t") #'nano-agenda-select-today)
    (local-set-key (kbd "q") #'nano-agenda-close)
    (local-set-key (kbd "<return>") #'nano-agenda-close)
    (local-set-key (kbd "<escape>") #'nano-agenda-cancel)))


;; --- Navigation ------------------------------------------------------
(defun nano-agenda-forward-day ()
  (interactive)
  (setq nano-agenda-selected (ts-inc 'day 1 nano-agenda-selected))
  (nano-agenda))

(defun nano-agenda-backward-day ()
  (interactive)
  (setq nano-agenda-selected (ts-dec 'day 1 nano-agenda-selected))
  (nano-agenda))

(defun nano-agenda-forward-week ()
  (interactive)
  (setq nano-agenda-selected (ts-inc 'day 7 nano-agenda-selected))
  (nano-agenda))

(defun nano-agenda-backward-week ()
  (interactive)
  (setq nano-agenda-selected (ts-dec 'day 7 nano-agenda-selected))
  (nano-agenda))

(defun nano-agenda-forward-month ()
  (interactive)
  (setq nano-agenda-selected (ts-inc 'month 1 nano-agenda-selected))
  (nano-agenda))

(defun nano-agenda-backward-month ()
  (interactive)
  (setq nano-agenda-selected (ts-dec 'month 1 nano-agenda-selected))
  (nano-agenda))
  
(defun nano-agenda-forward-year ()
  (interactive)
  (setq nano-agenda-selected (ts-inc 'year 1 nano-agenda-selected))
  (nano-agenda))
    
(defun nano-agenda-backward-year ()
  (interactive)
  (setq nano-agenda-selected (ts-dec 'year 1 nano-agenda-selected))
  (nano-agenda))

(defun nano-agenda-select-today ()
  (interactive)
  (setq nano-agenda-selected (ts-now))
  (nano-agenda))

(defun nano-agenda-close ()
  (interactive)
  (kill-buffer "*nano-agenda*"))

(defun nano-agenda-select ()
  (interactive)
  (kill-buffer "*nano-agenda*"))

(defun nano-agenda-cancel ()
  (interactive)
  (kill-buffer "*nano-agenda*"))


;; --- Display ---------------------------------------------------------
(defun nano-agenda-header-month (selected)
  (let* ((map-left (make-sparse-keymap))
         (map-right (make-sparse-keymap)))

    (define-key map-left (kbd "<down-mouse-1>") #'nano-agenda-backward-month)
    (define-key map-right (kbd "<down-mouse-1>") #'nano-agenda-forward-month)
    
    (concat
     (propertize "<" 'face 'nano-agenda-face-button
                     'mouse-face 'nano-agenda-face-mouse
                     'help-echo "Previous month"
                     'keymap map-left)

     (propertize (center-string (format "%s %d" (ts-month-name selected)
                                                (ts-year selected)) 18)
                 'face 'nano-agenda-face-month-name)
     
     (propertize ">" 'face 'nano-agenda-face-button
                     'mouse-face 'nano-agenda-face-mouse
                     'help-echo "Next month"
                     'keymap map-right)
     " ")))

(defun nano-agenda-header-names (selected)
  (propertize "Mo Tu We Th Fr Sa Su "
              'face 'nano-agenda-face-day-name))

(defun nano-agenda-body-days (selected)
  (let* ((today  (ts-now))
         (day    (ts-day   selected))
         (month  (ts-month selected))
         (year   (ts-year  selected))
         (start (make-ts :year year :month month :day 1
                         :hour 0 :minute 0 :second 0))
         (dow   (mod (+ 6 (ts-dow start)) 7))
         (start (ts-dec 'day dow start))
         (result ""))

    (dotimes (row 6)
      (dotimes (col 7)
        (let* ((day (+ (* row 7) col))
               (date (ts-inc 'day day start))
               (map (make-sparse-keymap))
               (is-today (and (= (ts-year date) (ts-year today))
                              (= (ts-doy date) (ts-doy today))))
               (is-selected (and (= (ts-year date) (ts-year selected))
                                 (= (ts-doy date) (ts-doy selected))))
               (is-selected-today (and is-selected is-today))
               (is-outday (not (= (ts-month date) month)))
               (is-holidays (calendar-check-holidays (list
                                                      (ts-month date)
                                                      (ts-day date)
                                                      (ts-year date))))
               (is-weekend (or (= (ts-dow date) 0) (= (ts-dow date) 6)))
               (face (cond (is-selected-today 'nano-agenda-face-selected-today)
                           (is-selected       'nano-agenda-face-selected)
                           (is-today          'nano-agenda-face-today)
                           (is-outday         'nano-agenda-face-outday)
                           (is-weekend        'nano-agenda-face-weekend)
                           (is-holidays       'nano-agenda-face-holidays)
                           (t                 'nano-agenda-face-default))))
          
            (define-key map (kbd "<down-mouse-1>")
              `(lambda() (interactive) (nano-agenda ,date)))
            (setq result (concat result
                                 (propertize (format "%2d" (ts-day date))
                                             'face face
                                             'mouse-face (cond (is-selected-today 'nano-agenda-face-selected-today)
                                                               (is-selected       'nano-agenda-face-selected)
                                                               (t 'nano-agenda-face-mouse))
                                             'help-echo (format "%s%s" (ts-format "%A %-e %B %Y" date)
                                                                (if is-holidays (format " (%s)" (nth 0 is-holidays))
                                                                     ""))
                                             'keymap map)
                                 " "))))
      (setq result (concat result "\n")))
    
    result))


;; ---------------------------------------------------------------------
(defun nano-agenda (&optional selected)
  (interactive)
  
  (if selected
      (setq nano-agenda-selected selected))

  ;; -- Create frame if necessary ---
  (condition-case nil
      (select-frame-by-name "*nano-agenda-frame*")
    (error
     (make-frame `((name . "*nano-agenda-frame*")
                   (unsplittable . t)
                   (buffer-predicate . (lambda (x) nil))
                   (internal-border-width . 0)
                   (minibuffer . nil)))))
    
  (select-frame-by-name "*nano-agenda-frame*")
  (modify-frame-parameters nil '((width .  72)
                                 (height . 10)))
  (switch-to-buffer (get-buffer-create "*nano-agenda*"))
  (set-window-dedicated-p (get-buffer-window ) t)

  ;; --- Display agenda ---
  (with-current-buffer (get-buffer-create "*nano-agenda*")
    (switch-to-buffer "*nano-agenda*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (set-window-margins nil 2)
      (face-remap-add-relative 'header-line
                               `(:family "Roboto Mono"
                                 :foreground ,nano-light-background
                                 :background ,nano-light-faded
                                 :weight regular
                                 :box (:line-width 2 :color "#ffffff" :style nil)))

      (setq mode-line-format nil)
      (setq header-line-format nil)

      (insert "\n")
      (insert (nano-agenda-header-month nano-agenda-selected))
      (insert "\n")
      (insert (nano-agenda-header-names nano-agenda-selected))
      (insert "\n")
      (insert (nano-agenda-body-days nano-agenda-selected))
      (insert "\n")
      
      ;; --- Org agenda entries ---
      (goto-char (point-min))
      (forward-line 1)
      (end-of-line)
      (let ((is-holidays (calendar-check-holidays (list
                                                   (ts-month nano-agenda-selected)
                                                   (ts-day nano-agenda-selected)
                                                   (ts-year nano-agenda-selected)))))

        ;; (setq header-line-format (concat
        ;;                           (propertize " " 'display '(raise +0.25))
        ;;                           " "
        ;;                           (ts-format "%A %-e %B %Y" nano-agenda-selected)
        ;;                           (if is-holidays (format " (%s)" (nth 0 is-holidays)) "")
        ;;                           (propertize " " 'display '(raise -0.35))))
        
        (insert (concat (propertize (ts-format "  %A %-e %B %Y" nano-agenda-selected)
                                    'face 'nano-agenda-face-month-name)
                        (propertize
                         (if is-holidays (format " (%s)" (nth 0 is-holidays)) "")
                         'face 'nano-agenda-face-holidays))))
      (forward-line 2)
      (end-of-line)
      (let* ((org-agenda-hide-tags-regexp ".")
             (date nano-agenda-selected)
             (file nano-agenda-file)
             (date (list (ts-month date) (ts-day date) (ts-year date)))
             (entries (cl-sort (org-agenda-get-day-entries file date :timestamp) 'string-lessp)))

        (setq num 0)
        (while (< num (min 5 (length entries)))
          (let* ((entry (nth num entries))
                 (text (substring-no-properties (format "%s" entry))))
            (insert (propertize
                     (truncate-string-to-width
                      (if (string-match org-link-bracket-re text)
                          (replace-match "[-]" nil nil text) text) 46 nil nil "…")
                     'face 'nano-face-salient)))
          (forward-line)
          (end-of-line)
          (setq num (1+ num))
          )
        (if (> (length entries) 5)
            (insert (propertize (format "  + %s non-displayed event(s)" (- (length entries) 5))
                    'face 'nano-agenda-face-holidays))))
      ;; --- End org agenda entries ---
      
      (goto-char (point-min)))    
    
    (let ((message-log-max)
          (is-holidays (calendar-check-holidays (list
                                                 (ts-month nano-agenda-selected)
                                                 (ts-day nano-agenda-selected)
                                                 (ts-year nano-agenda-selected)))))
      (message (format "%s%s" (ts-format "%A %-e %B %Y" nano-agenda-selected)
                       (propertize
                        (if is-holidays (format " (%s)" (nth 0 is-holidays)) "")
                        'face 'nano-agenda-face-holidays))))

    (nano-agenda-mode t)))

;;
(provide 'nano-agenda)
