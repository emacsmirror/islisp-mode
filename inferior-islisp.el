;;; inferior-islisp-.el ---  Run inferior ISLisp processes            -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Fermin Munoz

;; Author: Fermin Munoz
;; Maintainer: Fermin Munoz <fmfs@posteo.net>
;; Created: 24 Sep 2021
;; Version: 0.0.1
;; Keywords: islisp, lisp, programming
;; URL: 
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

;;; Code:

;;;; The requires

(require 'comint)
(require 'islisp-mode)
(require 'inf-lisp)

(defgroup inferior-islisp nil
  "Run a ISLisp process in a buffer."
  :group 'islisp)

(defcustom inferior-islisp-command-line '("/usr/local/bin/eisl")
  "Command line for calling an inferior islisp process."
  :type 'string
  :group 'inferior-islisp)

(defcustom inferior-islisp-command-line-args "-r"
  "Command line for calling an inferior islisp process."
  :type 'string
  :group 'inferior-islisp)

(defcustom inferior-islisp-source-modes '(islisp-mode)
  "List of modes which indicate a buffer contains ISLisp source code."
  :type '(repeat function)
  :group 'inferior-islisp)

(defcustom inferior-islisp-prompt "^> *"
  "Command line for calling an inferior islisp process."
  :type 'string
  :group 'inferior-islisp)

(defcustom inferior-islisp-load-command "(load \"%s\")\n"
  "Format-string for building a ISLisp expression to load a file.
This is NOT an standard function, but it is present in some
implementations."
  :type 'string
  :group 'inferior-islisp)

(defvar inferior-islisp-mode-hook '()
  "Hook for customising Inferior ISLisp mode.")

(defvar inferior-islisp-buffer nil "")

(defvar islisp-prev-l/c-dir/file nil
  "Record last directory and file used in loading or compiling.
This holds a cons cell of the form `(DIRECTORY . FILE)'
describing the last `islisp-load-file' or `islisp-compile-file' command.")

(defun inferior-islisp-proc ()
  (let ((proc (get-buffer-process (get-buffer inferior-islisp-buffer))))
    (or proc
	(error "No ISLisp subprocess; see variable `inferior-islisp-buffer'"))))

(defun islisp-eval-region (start end &optional _and-go)
  "Send the current region from START to END the inferior ISLisp process."
  (interactive "r\nP")
  (comint-send-region (inferior-islisp-proc) start end)
  (comint-send-string (inferior-islisp-proc) "\n"))

(defun islisp-eval-string (string)
  "Send STRING to the inferior Lisp process to be executed."
  (comint-send-string (inferior-islisp-proc) (concat string "\n")))

(defun islisp-do-defun (do-region)
  "Send the current defun to the inferior Lisp process."
  (save-excursion
    (beginning-of-defun)
    (forward-sexp)
    (let ((end (point)) (case-fold-search t))
      (beginning-of-defun)
      (funcall do-region (point) end))))

(defun islisp-eval-defun (&optional _and-go)
  "Send the current defun to the inferior ISLisp process."
  (interactive "P")
  (islisp-do-defun 'islisp-eval-region))

(defun islisp-eval-last-sexp (&optional _and-go)
  "Send the previous sexp to the inferior ISLisp process. "
  (interactive "P")
  (islisp-eval-region (save-excursion (backward-sexp) (point)) (point)))

(defun islisp-load-file (file-name)
  "Load a ISLisp file with FILE-NAME into the inferior ISLisp process."
  (interactive (comint-get-source "Load ISLisp file: " islisp-prev-l/c-dir/file
				  inferior-islisp-source-modes nil)) ; nil because LOAD
					; doesn't need an exact name
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq islisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (comint-send-string (inferior-islisp-proc)
		      (format inferior-islisp-load-command file-name))

  (comint-send-string (inferior-islisp-proc)
		      (format "(format (standard-output) \"Loaded: %s\")\n" file-name)))

(defun islisp-compile-file (file-name)
  "Compile a ISLisp FILE-NAME in the inferior ISLisp process."
  (interactive (comint-get-source "Compile ISLisp file: " islisp-prev-l/c-dir/file
				  inferior-islisp-source-modes nil)) ; nil = don't need
					; suffix .lisp
  (comint-check-source file-name) ; Check to see if buffer needs saved.
  (setq islisp-prev-l/c-dir/file (cons (file-name-directory    file-name)
				       (file-name-nondirectory file-name)))
  (comint-send-string (inferior-islisp-proc) (concat "(compile-file \"" file-name "\")\n")))

(defun inferior-islisp ()
  ""
  (interactive)
  (when (not (comint-check-proc "*inferior-islisp*"))
    (set-buffer (apply (function make-comint)
		       "inferior-islisp" inferior-islisp-command-line))
    (inferior-islisp-mode))
  (setq inferior-islisp-buffer "*inferior-islisp*")
  (pop-to-buffer-same-window "*inferior-islisp*"))

(define-derived-mode inferior-islisp-mode comint-mode "Inferior ISLisp"
  ""
  (islisp-mode-variables)
  (setq-local comint-prompt-regexp inferior-islisp-prompt
	      mode-line-process '(":%s")
	      comint-get-old-input (function lisp-get-old-input)
	      comint-prompt-read-only t))


(provide 'inferior-islisp)
;;; inferior-islisp.el ends here
