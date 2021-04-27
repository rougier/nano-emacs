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
(require 'svg-tag-mode)
(require 'mu4e-dashboard)
(require 'mu4e-thread-folding)

;; --- Dashboard --------------------------------------------------
(defun nano-mu4e-dashboard-colors ()
  (face-remap-add-relative 'org-link 'nano-face-default)
  (face-remap-add-relative 'org-verbatim 'nano-face-salient)
  (face-remap-add-relative 'org-level-1 'nano-face-default)
  (face-remap-add-relative 'org-level-2 'nano-face-default))
(add-hook 'mu4e-dashboard-mode-hook #'nano-mu4e-dashboard-colors)


;; --- Thread folding --------------------------------------------------
(setq mu4e-thread-folding-root-folded-prefix-string
      (concat
       (propertize "" 'face `(:foreground ,nano-color-faded :height 120))
       " "))
(setq mu4e-thread-folding-root-unfolded-prefix-string
      (concat
       (propertize "" 'face `(:foreground ,nano-color-faded :height 120))
       " "))
(setq mu4e-thread-folding-child-prefix-string
      "  ")

(setq mu4e-thread-folding-root-unfolded-face 'nano-face-default)
(setq mu4e-thread-folding-root-folded-face 'nano-face-default)
(setq mu4e-thread-folding-root-child-face 'nano-face-default)
(setq mu4e-thread-folding-default-view 'unfolded)


;; --- Headers view ----------------------------------------------------
(defun mu4e-get-account (msg)
  (let* ((maildir (mu4e-message-field msg :maildir))
         (maildir (substring maildir 1)))
    (nth 0 (split-string maildir "/"))))
    
(defun mu4e-get-maildir (msg)
  (let* ((maildir (mu4e-message-field msg :maildir))
         (maildir (substring maildir 1)))
    (nth 0 (reverse (split-string maildir "/")))))

(defun mu4e-get-mailbox (msg)
  (format "%s - %s" (mu4e-get-account msg) (mu4e-get-maildir msg)))

(defun mu4e-headers-tag (text tag face help query)
  "Make a clickable button with specified FACE displaying TEXT.
   When hovered, HELP is displayed. When clicked, mu4e QUERY is executed."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map 'mu4e-headers-mode-map)
    (define-key map [mouse-1] `(lambda ()
                                 (interactive) (mu4e-headers-search ,query)))
    (concat 
     (propertize text
                'display tag
                'face face
                'mouse-face `(:foreground ,nano-color-salient)
                'local-map map
                'help-echo `(lambda (window _object _point)
                              (let (message-log-max) (message ,help))))
     " ")))

(defun mu4e-headers-button (text face help query)
  "Make a clickable button with specified FACE displaying TEXT.
   When hovered, HELP is displayed. When clicked, mu4e QUERY is executed."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map 'mu4e-headers-mode-map)
    (define-key map [mouse-1] `(lambda ()
                                 (interactive) (mu4e-headers-search ,query)))
    (propertize text
                'face face
                'mouse-face `(:foreground ,nano-color-background
                              :background ,nano-color-faded)
                'local-map map
                'help-echo `(lambda (window _object _point)
                              (let (message-log-max) (message ,help))))))

(defun mu4e-headers-date-button (date face)
  (concat
   (mu4e-headers-button (format-time-string "%d" date)
                        face
                        (format-time-string "Mails from %d %B %Y" date)
                        (format-time-string "date:%Y%m%d" date))
   (propertize "/" 'face face)
   (mu4e-headers-button (format-time-string "%m" date)
                        face
                        (format-time-string "Mails from %B %Y" date)
                        (format-time-string "date:%Y%m" date))
   (propertize "/" 'face face)
   (mu4e-headers-button (format-time-string "%Y" date)
                        face
                        (format-time-string "Mails from %Y" date)
                        (format-time-string "date:%Y" date))))

(defun mu4e-headers-is-today (date)
  (= (- (time-to-days (current-time)) (time-to-days date)) 0))

(defun mu4e-headers-is-yesterday (date)
  (= (- (time-to-days (current-time)) (time-to-days date)) 1))
  
(defun mu4e-headers-relative-date (msg)
  (let* ((thread  (mu4e-message-field msg :thread))
         (level (plist-get thread :level))
         (empty-parent (and thread (plist-get thread :empty-parent)))
         (child   (and thread (> (plist-get thread :level) 0)))
         (unread  (memq 'unread  (mu4e-message-field msg :flags)))
         (date (mu4e-msg-field msg :date))
         (diff (- (time-to-days (current-time)) (time-to-days date)))
         (face 'nano-face-salient))

    ;; (setq face (cond ((= diff   0) (if unread 'nano-face-strong 'nano-face-faded))
    ;;                 ((< diff   8) (if unread 'nano-face-strong 'nano-face-faded))
    ;;                 (t            (if unread 'nano-face-strong 'nano-face-faded))))
    ;; (setq face (if unread 'nano-face-strong 'nano-face-faded))
    (setq face 'nano-face-faded)
    ;; (if (and (not empty-parent) child)
    ;;    (setq face `(:foreground ,nano-color-subtle)))

    (cond ((mu4e-headers-is-today date)
           (mu4e-headers-button (format-time-string "     %H:%M" date)
                                face
                                (format-time-string "Mails from today")
                                (format-time-string "date:%Y%m%d" date)))
          ((mu4e-headers-is-yesterday date)
           (mu4e-headers-button " Yesterday"
                                face
                                (format-time-string "Mails from yesterday")
                                (format-time-string "date:%Y%m%d" date)))
          (t  (mu4e-headers-date-button date face)))))

           
(defun mu4e-headers-sender-subject (msg)
  (let* ((thread  (mu4e-message-field msg :thread))
         (level (plist-get thread :level))
         (empty-parent (and thread (plist-get thread :empty-parent)))
         (child   (and thread (> (plist-get thread :level) 0)))
         (flagged (memq 'flagged  (mu4e-message-field msg :flags)))
         (unread  (memq 'unread  (mu4e-message-field msg :flags)))
         (replied (memq 'replied (mu4e-message-field msg :flags)))
         (maildir (mu4e-get-maildir msg))
         (inbox   (string= maildir "inbox"))
         (attachment (memq 'attach (mu4e-message-field msg :flags)))
         (sent    (string= maildir "sent"))
         (tags    (mu4e-message-field msg :tags))
         (list    (mu4e-message-field msg :mailing-list))
         (sender-name     (or (car (car (mu4e-message-field msg :from)))))
         (sender-address  (or (cdr (car (mu4e-message-field msg :from)))))
         (subject (mu4e-message-field msg :subject)))
    (concat
     (if (and (not empty-parent) child)
     ;; (if (> level 0)
         (propertize "│ " 'face `(:foreground ,nano-color-subtle :height 130)))

     (mu4e-headers-button
      (or sender-name sender-address "Unknown")
      (cond  ((and child unread) 'nano-face-default)
             (child              'nano-face-faded)
             (unread             'nano-face-strong)
             (t                  'nano-face-strong))
      
      (format "Mails from %s" sender-address)
      (concat "from:" sender-address))

     (cond (unread ;;(propertize " ● " 'face 'nano-face-default))
            (propertize " ● " 'face '(:height 130)))
           (inbox
            (propertize " • " 'face 'nano-face-faded))
           ((or (not child) empty-parent tags)
            (propertize " - " 'face 'nano-face-faded)))


     (if (or (not mu4e-headers-show-threads) (not child))
         (if list (concat (mu4e-headers-button
                           "[LIST]"
                           '(:inherit nano-face-faded)
                           (format "Mails from/to %s" list)
                           (format "list:%s" list)) " ")))

     (if tags (concat
               (mapconcat
                (function (lambda (tag)
                            (mu4e-headers-button
                             (format "[%s]" tag)
                             '(:inherit (nano-face-salient nano-face-strong))
                             (format "Mails with tag %s" tag)
                             (concat "tag:" tag))))  tags " ") " "))

     (if (or (not child) empty-parent)
     ;; (if (= level 0)
         (propertize subject 'face 'nano-face-default)))))

(defun mu4e-headers-attach (msg)
  (cond ((memq 'flagged  (mu4e-message-field msg :flags))
         (propertize "!" 'face 'nano-face-strong))
        ((memq 'attach  (mu4e-message-field msg :flags))
         (propertize "" 'face 'nano-face-faded))
        (t " ")))


(add-to-list 'mu4e-header-info-custom
             '(:sender-subject . (:name "Sender and subject"
                                 :shortname ""
                                 :function mu4e-headers-sender-subject)))

(add-to-list 'mu4e-header-info-custom
             '(:attach . (:name "Attachment"
                          :shortname ""
                          :function mu4e-headers-attach)))

(add-to-list 'mu4e-header-info-custom
             '(:relative-date . (:name "Relative date"
                                 :shortname ""
                                 :function mu4e-headers-relative-date)))

(add-to-list 'mu4e-header-info-custom
             '(:empty . (:name "Empty"
                          :shortname ""
                          :function (lambda (msg) " "))))


(setq mu4e-headers-fields  '((:empty          .  1)
                             (:sender-subject . 68)
                             (:relative-date  . 10)
                             (:attach         .  1)))

(setq mu4e-headers-window-width nil)
(setq mu4e-headers-window-resize-timer nil)
 
(defun mu4e-resize-headers-window (frame)
  (when mu4e-headers-window-resize-timer
    (cancel-timer mu4e-headers-window-resize-timer)
    (setq mu4e-headers-window-resize-timer nil))
  (setq mu4e-headers-window-resize-timer
        (run-with-timer 0.5 nil #'-mu4e-resize-headers-window)))

(defun -mu4e-resize-headers-window ()
  (if (get-buffer-window "*mu4e-headers*" t)
      (let ((width (window-width (get-buffer-window "*mu4e-headers*" t))))
        (when (and width (not (eq mu4e-headers-window-width width)))
          (setq mu4e-headers-fields `((:empty . 1)
                                      (:sender-subject . ,(- width 18))
                                      (:relative-date  .  10)
                                      (:attach . 1)
                                      ))
          (setq mu4e-headers-window-width width)
          (mu4e-headers-rerun-search)))))

(add-hook 'window-size-change-functions  #'mu4e-resize-headers-window)




;; --- Nicer actions display using SVG tags -----------------------------------
(plist-put (cdr (assq 'refile   mu4e-marks)) :char "×")
(plist-put (cdr (assq 'trash    mu4e-marks)) :char "×")
(plist-put (cdr (assq 'untrash  mu4e-marks)) :char "×")
(plist-put (cdr (assq 'unread   mu4e-marks)) :char "×")
(plist-put (cdr (assq 'delete   mu4e-marks)) :char "×")
(plist-put (cdr (assq 'flag     mu4e-marks)) :char "×")
(plist-put (cdr (assq 'unflag   mu4e-marks)) :char "×")
(plist-put (cdr (assq 'move     mu4e-marks)) :char "×")
(plist-put (cdr (assq 'tag      mu4e-marks)) :char "×")
(setq mu4e-headers-show-target nil)
(set-face-attribute 'mu4e-header-marks-face nil
                    :inherit 'nano-face-strong
                    :foreground nano-color-foreground)

(defface mu4e-headers-svg-tag-default-face
  `((t :foreground ,nano-color-background
       :background ,nano-color-faded
       :box (:line-width 1 :color ,nano-color-faded :style nil)
       :family ,(face-attribute 'default :family)
       :weight ,(face-attribute 'default :weight)
       :height ,(if (display-graphic-p)
                    (- (face-attribute 'default :height) 20)
                  1)))
  "SVG tag default face"
  :group 'svg-tag)

(defface mu4e-headers-svg-tag-popout-face
  `((t :foreground ,(face-attribute 'default :background)
       :background ,nano-color-popout
       :box nil
       :family ,(face-attribute 'default :family)
       :weight ,(face-attribute 'default :weight)
       :height ,(if (display-graphic-p)
                    (- (face-attribute 'default :height) 20)
                  1)))
  "SVG tag popout face"
  :group 'mu4e)

(defface mu4e-headers-svg-tag-salient-face
  `((t :foreground ,nano-color-background
       :background ,nano-color-salient
       :box (:line-width 1 :color ,nano-color-salient :style nil)
       :family ,(face-attribute 'default :family)
       :weight ,(face-attribute 'default :weight)
       :height ,(if (display-graphic-p)
                    (- (face-attribute 'default :height) 20)
                  1)))
  "SVG tag salient face"
  :group 'svg-tag)

(defface mu4e-headers-svg-tag-critical-face
  `((t :foreground ,nano-color-background
       :background ,nano-color-critical
       :box (:line-width 1 :color ,nano-color-critical :style nil)
       :family ,(face-attribute 'default :family)
       :weight ,(face-attribute 'default :weight)
       :height ,(if (display-graphic-p)
                    (- (face-attribute 'default :height) 20)
                  1)))
  "SVG tag critical face"
  :group 'svg-tag)

(defun mu4e-mark-at-point-advice (mark target)
  (interactive)
  (let* ((msg (mu4e-message-at-point))
         (docid (mu4e-message-field msg :docid))
         (overlay (make-overlay (- (line-end-position) 12)
                                (- (line-end-position) 2))))
    (save-excursion
      ;; (remove-overlays (line-beginning-position) (line-end-position))
      (delete-overlay (make-overlay (line-beginning-position) (line-end-position)))
      (if (eql mark 'unmark)
          (delete-overlay overlay)
        (cond ((eql mark 'refile)
               (overlay-put overlay 'display (svg-tag-make "ARCHIVE" 'mu4e-headers-svg-tag-salient-face 3 0)))
              ((eql mark 'trash)
               (overlay-put overlay 'display (svg-tag-make "TRASH" 'mu4e-headers-svg-tag-critical-face 5 0)))
              ((eql mark 'untrash)
               (overlay-put overlay 'display (svg-tag-make "UNTRASH" 'mu4e-headers-svg-tag-default-face 3 0)))
              ((eql mark 'delete)
               (overlay-put overlay 'display (svg-tag-make "DELETE" 'mu4e-headers-svg-tag-critical-face 4 0)))
              ((eql mark 'unread)
               (overlay-put overlay 'display (svg-tag-make "UNREAD" 'mu4e-headers-svg-tag-default-face 4 0)))
              ((eql mark 'flag)
               (overlay-put overlay 'display (svg-tag-make "FLAG" 'mu4e-headers-svg-tag-default-face 6 0)))
              ((eql mark 'unflag)
               (overlay-put overlay 'display (svg-tag-make "UNFLAG" 'mu4e-headers-svg-tag-default-face 4 0)))
              ((eql mark 'move)
               (overlay-put overlay 'display (svg-tag-make "MOVE" 'mu4e-headers-svg-tag-salient-face 6 0)))
              ((eql mark 'tag)
               (overlay-put overlay 'display (svg-tag-make "TAG" 'mu4e-headers-svg-tag-popout-face 7 0))))))))

(advice-add 'mu4e-mark-at-point :after #'mu4e-mark-at-point-advice)

(provide 'nano-mu4e)

