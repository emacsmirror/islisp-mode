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

(defvar inferior-islisp-mode-hook '()
  "Hook for customising Inferior ISLisp mode.")

(defvar inferior-islisp-buffer nil "")

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
  (setf comint-prompt-regexp inferior-islisp-prompt
	mode-line-process '(":%s"))
  (islisp-mode-variables)
  (setq comint-get-old-input (function lisp-get-old-input))
  (setq comint-input-filter (function lisp-input-filter)))


(provide 'inferior-islisp)
