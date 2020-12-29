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

;; We have a local copy of smex (that has not changed since 2014)
(require 'smex)

(setq ivy-height 4)
(setq ivy-count-format "")
(setq ivy-initial-inputs-alist: '((counsel-minor .            "^+")
                                  (counsel-package .          "^+")
                                  (counsel-org-capture .      "^")
                                  (counsel-M-x .              "^")
                                  (counsel-describe-symbol .  "^")
                                  (org-refile .               "") 
                                  (org-agenda-refile .        "")
                                  (org-capture-refile .       "")
                                  (Man-completion-table .     "^")
                                  (woman .                    "^")))
(global-set-key (kbd "M-x")     'counsel-M-x)


(global-set-key (kbd "C-c r")   'counsel-recentf)
(global-set-key (kbd "C-c b")   'counsel-bookmark)
(global-set-key (kbd "C-x C-b") 'counsel-switch-buffer)
(global-set-key (kbd "C-c c")   'counsel-org-capture)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)


(provide 'nano-counsel)
