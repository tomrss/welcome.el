;;; welcome.el --- Welcome screen -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com>
;; Maintainer: Tommaso Rossi <tommaso.rossi1@protonmail.com>
;; Created: 2022
;; Version: 0.1
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1")(all-the-icons "5.0.0"))
;; Homepage: https://github.com/tomrss/welcome.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Simple and lightweight welcome screen.
;;
;; It consists in three main sections: an header displaying a banner
;; and some startup info, a menu that expose some utilities (by
;; default recent files, projects and other), and a footer with some
;; build info.  This package SHOULD just offer a function for creating
;; a simple welcome buffer with a very minimal dedicated major mode.
;; It SHOULD not do anything strange like registering hooks or other.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'all-the-icons)

(defgroup welcome nil
  "Welcome buffer."
  :group 'convenience
  :prefix "welcome-")

(defcustom welcome-buffer-name "*Welcome*"
  "Name of the welcome buffer."
  :type 'string)

;; shamelessly stolen from dashboard.el (not the only thing)
(defcustom welcome-init-info
  (lambda ()
    (let ((package-count 0) (time (emacs-init-time)))
      (when (bound-and-true-p package-alist)
        (setq package-count (length package-activated-list)))
      (when (boundp 'straight--profile-cache)
        (setq package-count
              (+ (hash-table-count straight--profile-cache) package-count)))
      (if (zerop package-count)
          (format "Emacs started in %s" time)
        (format "%d packages loaded in %s" package-count time))))
  "Init info with packages loaded and init time."
  :type '(function string)
  :group 'dashboard)

;; TODO rename this horror
(defcustom welcome-window-width 80
  "Rewrite this please."
  :type 'integer)

(defcustom welcome-message "Welcome to GNU Emacs."
  "Welcome message."
  :type 'string)

(defcustom welcome-separator
  (concat (make-string welcome-window-width ?\u2500)
          "\n\n")
  "Separator between welcome sections."
  :type 'string)

;; TODO asset in welcome folder
(defcustom welcome-banner
  (concat (file-name-directory (locate-library "welcome")) "asset/emacs.png")
  ;; (expand-file-name "asset/emacs.png" user-emacs-directory)
  "Path of the image banner to show in welcome buffer."
  :type 'string)

(defcustom welcome-menu-items
  '(("Recent files"
     :key "f"
     :action recentf-open-files
     :icon "history")
    ("Projects"
     :key "p"
     :action project-switch-project
     :icon "code")
    ("Dired"
     :key "d"
     :action dired
     :icon "file-directory")
    ("Edit configuration"
     :key "c"
     :action (lambda ()
               (interactive)
               (find-file user-init-file))
     :icon "gear")
    ("Eshell"
     :key "e"
     :action eshell
     :icon "terminal"))
  "Items to show in the welcome menu section."
  :type 'list)

(defgroup welcome-faces nil
  "Faces used by Welcome."
  :group 'welcome
  :group 'faces)

(defface welcome-message-face '((t (:inherit default)))
  "Face used for the welcome message.")

(defface welcome-startup-info-face '((t :inherit font-lock-comment-face))
  "Face used for the startup info in the welcome header.")

(defface welcome-menu-item-face '((t (:inherit font-lock-keyword-face)))
  "Face used for the welcome menu items.")

(defface welcome-footer-build-info-face '((t (:inherit font-lock-comment-face)))
  "Face used for build info in the footer.")

(defvar welcome-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'welcome-open-menu-item-at-point)
    map)
  "Keymap used in welcome screen.")

;; TODO horrible function
(defun welcome-pad-for-centering (width)
  "Return padding for centering content with WIDTH."
  (make-string
   (max 0
        (floor (/ (- welcome-window-width width) 2)))
   ?\ ))

;; TODO don't like this either
(defun welcome-insert-centered-line (str)
  "Insert a centered line with content STR." 
  (insert (welcome-pad-for-centering (length str)))
  (insert str)
  (newline))
;; (defun welcome-insert-centered-line (str)
;;   (let ((fill-column welcome-window-width))
;;     (insert str)
;;     (center-line)
;;     (newline)))

(defun welcome-insert-image (image)
  "Insert IMAGE."
  (unless (file-exists-p image)
    (error "Image file %s does not exist" image))
  (let* ((spec
          (apply 'create-image image nil nil nil))
         (size (when (fboundp 'image-size) (image-size spec)))
         (width (car size)))
    (goto-char (point-min))
    (insert "\n")
    (insert (welcome-pad-for-centering width))
    (insert-image spec)
    (insert "\n\n")))

(defun welcome-open-menu-item-at-point (&optional point)
  "Open the menu item at point by launching related action.

Menu items are specified in `welcome-menu-items'.
If no matching menu item is found in the current line, do nothing.
If POINT is non-nil, open menu item at POINT."
  (interactive)
  (save-excursion
    (when point
      (goto-char point))
    (let* ((beg (line-beginning-position))
           (end (line-end-position))
           (line (buffer-substring-no-properties beg end)))
      (when-let ((matched-item (car (seq-filter
                                     (lambda (menu-item)
                                       (string-match (car menu-item) line))
                                     welcome-menu-items))))
        (call-interactively (plist-get (cdr matched-item) :action))))))

(defun welcome-menu-item (menu-item)
  "Render the MENU-ITEM and register associated key in local map."
  (let* ((title (car menu-item))
         (entry-plist (cdr menu-item))
         (key (plist-get entry-plist :key))
         (action (plist-get entry-plist :action))
         (icon (plist-get entry-plist :icon))
         (all-the-icons-scale-factor 1.45)
         (all-the-icons-default-adjust -0.02))
    ;; TODO why this doesn't work here?
    ;; (evil-define-key 'normal welcome-mode-map (kbd key) action)
    (define-key welcome-mode-map (kbd key) action)
    (insert "    ")
    (when icon
      (insert (all-the-icons-octicon icon :face 'welcome-menu-item-face))
      (insert "  "))
    (insert (propertize (format "(%s)  " key) 'face 'font-lock-comment-face))
    ;; TODO do it properly with align-regexp (but seems broken with icons)
    (insert (make-string (max 0 (- 5 (length key))) ?\ ))
    (insert (propertize title 'face 'welcome-menu-item-face))
    (newline)))

(defun welcome-header ()
  "Render the welcome header."
  (newline)
  (welcome-insert-image welcome-banner)
  (goto-char (point-max))
  (let ((init-info (if (functionp welcome-init-info)
                       (funcall welcome-init-info)
                     welcome-init-info)))
    (welcome-insert-centered-line
     (propertize welcome-message 'face 'welcome-message-face))
    (welcome-insert-centered-line
     (propertize init-info 'face 'welcome-startup-info-face)))
  (insert "\n\n"))

(defun welcome-menu ()
  "Render the welcome menu."
  (dolist (menu-item welcome-menu-items)
    (welcome-menu-item menu-item))
  (insert (make-string (max 1 (- 13 (length welcome-menu-items))) ?\n)))

(defun welcome-footer ()
  "Render the welcome screen footer."
  (let ((version-line (emacs-version)))
    (when (string-match "^\\(.*\\)(\\(.*\\))" version-line)
      (let ((version-info (match-string 1 version-line))
            (build-info (match-string 2 version-line)))
        (insert (welcome-pad-for-centering (+ 3 (length version-info))))
        (insert (all-the-icons-fileicon "elisp" :face 'welcome-menu-item-face))
        (insert "  ")
        (insert (propertize version-info 'face 'welcome-message-face))
        (newline)
        (welcome-insert-centered-line
         (propertize build-info 'face 'welcome-footer-build-info-face))))))

(defun welcome-goto-first-menu-item ()
  "Bring the point on the first menu item."
  (goto-char (point-min))
  (when-let ((first-menu-item-pos
              (search-forward (car (car welcome-menu-items)) nil t)))
    (goto-char first-menu-item-pos)
    (beginning-of-visual-line)))

(define-derived-mode welcome-mode special-mode "Welcome"
  "Simple mode for welcome screen.")

(defun welcome-screen-create (buf-name)
  "Create the welcome screen in a buffer named BUF-NAME."
  (with-current-buffer (get-buffer-create buf-name)
    (welcome-header)
    (insert welcome-separator)
    (welcome-menu)
    (insert welcome-separator)
    (welcome-footer)
    (welcome-goto-first-menu-item)
    (welcome-mode)))

;;;###autoload
(defun welcome-screen ()
  "Open the welcome screen."
  (interactive)
  (unless (get-buffer welcome-buffer-name)
    (welcome-screen-create welcome-buffer-name))
  (switch-to-buffer welcome-buffer-name))

(provide 'welcome)
;;; welcome.el ends here
