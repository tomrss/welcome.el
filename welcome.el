;;; welcome.el --- Welcome screen -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Tommaso Rossi

;; Author: Tommaso Rossi <tommaso.rossi1@protonmail.com>
;; Maintainer: Tommaso Rossi <tommaso.rossi1@protonmail.com>
;; Created: 2022
;; Version: 0.2.0
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1")(nerd-icons "0.1.0"))
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

(require 'subr-x)
(require 'nerd-icons)

(defgroup welcome nil
  "Welcome buffer."
  :group 'convenience
  :prefix "welcome-")

(defcustom welcome-buffer-name "*Welcome*"
  "Name of the welcome buffer."
  :type 'string)

(defcustom welcome-init-info
  (lambda ()
    (format "Loaded in %s with %d garbage collections"
            (emacs-init-time)
            gcs-done))
  "Init info."
  :type '(function string))

;; TODO rename this horror
(defcustom welcome-window-width 80
  "Rewrite this please."
  :type 'integer)

(defcustom welcome-message "Welcome to GNU Emacs."
  "Welcome message."
  :type 'string)

(defcustom welcome-separator
  (concat (propertize (make-string
                       (- welcome-window-width (if (display-graphic-p) 0 1)) ?\u2500)
                      'face
                      'font-lock-comment-face)
          "\n\n")
  "Separator between welcome sections."
  :type 'string)

(defcustom welcome-banner
  (concat (file-name-directory (locate-library "welcome")) "asset/emacs.png")
  "Path of the image banner to show in welcome buffer."
  :type 'string)

(defcustom welcome-menu-items
  '(("Recent files"
     :key "f"
     :action recentf-open-files
     :icon (nerd-icons-octicon . "nf-oct-history"))
    ("Projects"
     :key "p"
     :action project-switch-project
     :icon (nerd-icons-octicon . "nf-oct-repo"))
    ("Dired"
     :key "d"
     :action dired
     :icon (nerd-icons-sucicon . "nf-custom-folder_oct"))
    ("Edit configuration"
     :key "c"
     :action (lambda ()
               (interactive)
               (project-switch-project user-emacs-directory))
     :icon (nerd-icons-octicon . "nf-oct-gear"))
    ("Eshell"
     :key "e"
     :action eshell
     :icon (nerd-icons-octicon . "nf-oct-terminal"))
    ("Bookmarks"
     :key "b"
     :action bookmark-jump
     :icon (nerd-icons-octicon . "nf-oct-bookmark"))
    ("EWW browser"
     :key "w"
     :action eww
     :icon (nerd-icons-octicon . "nf-oct-globe")))
  "Items to show in the welcome menu section."
  :type 'list)

(defcustom welcome-icons-scale-factor 1.0
  "Scale factor of the icons showed in menu items.
It is used as local value for `nerd-icons-scale-factor'."
  :type 'number)

(defcustom welcome-icons-adjust 0.0
  "Vertical adjustment of the icons showed in menu items.
It is used as local value for `nerd-icons-default-adjust'."
  :type 'number)

(defgroup welcome-faces nil
  "Faces used by Welcome."
  :group 'welcome
  :group 'faces)

(defface welcome-message-face '((t (:inherit default)))
  "Face used for the welcome message.")

(defface welcome-startup-info-face '((t :inherit font-lock-comment-face :slant normal))
  "Face used for the startup info in the welcome header.")

(defface welcome-menu-item-face '((t (:inherit font-lock-keyword-face)))
  "Face used for the welcome menu items.")

(defface welcome-footer-build-info-face '((t (:inherit font-lock-comment-face :slant normal)))
  "Face used for build info in the footer.")

(defvar welcome-mode-map (make-sparse-keymap)
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

(defun welcome-menu-item (menu-item)
  "Render the MENU-ITEM and register associated key in local map."
  (let* ((title (car menu-item))
         (entry-plist (cdr menu-item))
         (key (plist-get entry-plist :key))
         (action (plist-get entry-plist :action))
         (icon (plist-get entry-plist :icon))
         (all-the-icons-scale-factor welcome-icons-scale-factor)
         (all-the-icons-default-adjust welcome-icons-adjust))
    (define-key welcome-mode-map (kbd key) action)
    (insert "    ")
    (insert
     (if (and icon (display-graphic-p))
         (funcall (car icon) (cdr icon) :face 'welcome-menu-item-face)
       " "))
    (insert "\t")
    (insert (propertize (format "(%s)  " key) 'face 'font-lock-comment-face))
    (insert (make-string (max 0 (- 5 (length key))) ?\ ))
    (insert-text-button title 'action (lambda (b)
                                        (call-interactively action)))
    (newline)))

(defun welcome-header ()
  "Render the welcome header."
  (newline)
  (if (display-graphic-p)
      (welcome-insert-image welcome-banner)
    (dotimes (number 10)
      (newline)))
  (goto-char (point-max))
  (let ((init-info (if (functionp welcome-init-info)
                       (funcall welcome-init-info)
                     welcome-init-info)))
    (welcome-insert-centered-line
     (propertize welcome-message 'face 'welcome-message-face))
    (newline)
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
  (let ((version-info (format "GNU Emacs %s" emacs-version))
        (build-info (format "build %s for %s%s"
                            emacs-build-number
                            system-configuration
                            (if emacs-build-time
		                        (format-time-string
					             " of %Y-%m-%d"
                                 emacs-build-time)
		                      ""))))
    (insert (welcome-pad-for-centering (+ 3 (length version-info))))
    (when (and (featurep 'nerd-icons)
               (display-graphic-p))
      (insert (nerd-icons-sucicon "nf-custom-emacs" :face 'welcome-menu-item-face)))
    (insert "  ")
    (insert (propertize version-info 'face 'welcome-message-face))
    (newline)
    (welcome-insert-centered-line
     (propertize build-info 'face 'welcome-footer-build-info-face))
    (newline)
    (welcome-insert-centered-line
     (propertize emacs-copyright 'face 'welcome-footer-build-info-face))))

(defun welcome-goto-first-menu-item ()
  "Bring the point on the first menu item."
  (goto-char (point-min))
  (let* ((first-menu-item (car (car welcome-menu-items)))
         (first-menu-item-pos (search-forward first-menu-item nil t)))
    (when first-menu-item-pos
      (goto-char first-menu-item-pos)
      (backward-char (length first-menu-item)))))

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
