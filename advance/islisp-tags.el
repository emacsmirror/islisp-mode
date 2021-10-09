;;; islisp-tags.el ---  ISLisp tags support            -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Fermin Munoz

;; Author: Fermin Munoz
;; Maintainer: Fermin Munoz <fmfs@posteo.net>
;; Created: 24 Sep 2021
;; Version: 0.2.0
;; Keywords: islisp, lisp, programming
;; URL: https://gitlab.com/sasanidas/islisp-mode
;; Package-Requires: ((emacs "26.3"))
;; License: GPL-3.0-or-later

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

;; Quick intro
;;
(require 'etags)
(require 'project)
(require 'cl-lib)

;;; Code:

;; (defcustom islisp-tags-regex '("'{lisp}/[ \t]*defglobal_[A-Z_ \t(]+\"\([^\"]+\)\"/\1/'"
;; 			       "'{lisp}/[ \t]*defdynamic_[A-Z_ \t(]+\"\([^\"]+\)\"/\1/'")
;;   "ISLisp TAGS regex, they are use to create/update the TAGS."
;;   :type 'list
;;   :group 'islisp)

(defcustom islisp-tags-tag-library t
  "Generate tags for library directory."
  :type 'boolean
  :group 'islisp)

;; (defun islisp-tags--regex ()
;;   (let (regex-str)
;;     (cl-loop for reg in islisp-tags-regex
;; 	     do (setf regex-str
;; 		      (concat regex-str " " (format "--regex=%s" reg))))
;;     regex-str))

(defun islisp-tags--generate-in (directory &optional tags-location append)
  (let ((etags-format "etags --language=lisp %s" ))
    (when tags-location (setf etags-format (concat etags-format
						   (format " -o %s" tags-location))))
    (when append (setf etags-format (concat etags-format " --append")))
    (shell-command-to-string (format "cd %s && find . -name \"*.lsp\" -print | %s -"
				     directory
				     etags-format))))
(defun islisp-tags-generate ()
  "Generate current project tags, created with `etags'."
  (interactive)
  (let* ((project (project-current t)))
    (islisp-tags--generate-in (cdr project))
    (when islisp-tags-tag-library
      (islisp-tags--update-library (cdr project)))))

(defun islisp-tags--beginning ()
  "Return the name of the position."
  (save-excursion
    (backward-word)
    (point)))

(defun islisp-tags-autocomplete ()
  "Show auto-completion at point using current tags table."
  (interactive)
  (let ((current-prefix (buffer-substring-no-properties (islisp-tags--beginning) (point)))
	(tags-table (tags-completion-table)))
    (completion-in-region (islisp-tags--beginning) (point) tags-table)))

(defun islisp-tags-symbols-navigate ()
  "Find the workspace TAG definition."
  (interactive)
  (xref-find-definitions
   (completing-read "Select tag symbol:" (tags-completion-table))))


(defun islisp-tags--update-library (root)
  (if (boundp easy-islisp-library-directory)
      (islisp-tags--generate-in easy-islisp-library-directory (concat root "TAGS") t)
    (error "The current implementation doesn't support library files.")))

(define-key islisp-mode-map (kbd "C-c C-w") 'islisp-tags-symbols-navigate)
(define-key islisp-mode-map (kbd "C-c TAB") 'islisp-tags-autocomplete)
(define-key islisp-mode-map (kbd "C-c C-e") 'islisp-tags-generate)




;;;; The requires
(provide 'islisp-tags)
;;; islisp-tags.el ends here
