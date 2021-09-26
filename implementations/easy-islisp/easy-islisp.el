
;;; easy-islisp.el ---  Specific Easy-ISLisp functionality.            -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Fermin Munoz

;; Author: Fermin Munoz
;; Maintainer: Fermin Munoz <fmfs@posteo.net>
;; Created: 24 Sep 2021
;; Version: 0.2.0
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
(require 'font-lock)
(require 'islisp-mode)
(require 'cl-lib)

(defgroup easy-islisp nil
  "Easy-ISLisp support."
  :group 'islisp)

(defcustom easy-islisp-executable "/usr/local/bin/eisl"
  "Absolute path of the Easy-ISLisp executable."
  :type '(file :must-match t)
  :group 'easy-islisp)

(defcustom easy-islisp-repl-debug-p t
  "Whether to enable debug mode in the REPL."
  :type 'boolean
  :group 'easy-islisp)


(defvar easy-islisp-keywords
  '("defmodule" "defpublic" "modulesubst" "trace" "untrace"
    "time" "random-real" "random" "heapdump" "instance"
    "nconc" "fast-address" "macroexpand-1" "macroexpand-all"
    "backtrace"
    "break" "edit" "set-editor" "wiringpi-setup-gpio"
    "delay-microseconds"
    "wiringpi-spi-setup-ch-speed" "pwm-set-mode" "pwm-set-range"
    "pwm-set-clock" "pin-mode" "digital-write" "digital-read"
    "pwm-write" "pull-up-dn-control" "delay" "compile-file"
    "compile-cuda" "formatter"
    "c-include" "c-define" "c-lang" "c-option"
    "gpu-mult" "gpu-add" "gpu-sub" "gpu-smult" "gpu-emult"
    "gpu-convolute" "gpu-deconvolute" "gpu-transpose"
    "gpu-ident" "gpu-full" "gpu-unfull" "gpu-accuracy" "gpu-correct"
    "gpu-activate" "gpu-trace"
    "gpu-loss" "gpu-average" "gpu-sum" "gpu-diff" "gpu-dropout"
    "gpu-gradfilter"
    "gpu-sgd" "gpu-momentum" "gpu-adagrad" "gpu-rms" "gpu-adam"
    "gpu-pooling" "gpu-unpooling"
    "gpu-random-select" "gpu-nanalizer" "gpu-copy"))

(defun easy-islisp-init ()
  "Easy-ISLisp initialisation function."
  (let ((new-font-lock (cl-concatenate
			'list islisp-general-keywords easy-islisp-keywords)))
    (setf islisp-font-lock-keywords
	  `((,(concat "(" (regexp-opt new-font-lock t) "\\_>")
	     (1 'font-lock-keyword-face t))
	    (,(concat "\\_<:" islisp-mode-symbol-regexp "\\_>")
	     (0 'font-lock-type-face t))
	    (,(concat "(" (regexp-opt '("cerror") t) "\\_>")
	     (1 'font-lock-warning-face))
	    (,(concat "(" islisp-def-keywords-regex "\\_>"
		      ;; Any whitespace and defined object.
		      "[ \t']*"
		      "\\(([ \t']*\\)?" ;; An opening paren.
		      "\\(\\(setf\\)[ \t]+" lisp-mode-symbol-regexp
		      "\\|" lisp-mode-symbol-regexp "\\)?")
	     (1 'font-lock-keyword-face)
	     (3 'font-lock-variable-name-face nil t))
	    (,(concat "(" islisp-func-def-keywords-regex "\\_>"
		      ;; Any whitespace and defined object.
		      "[ \t']*"
		      "\\(([ \t']*\\)?" ;; An opening paren.
		      "\\(\\(setf\\)[ \t]+" lisp-mode-symbol-regexp
		      "\\|" lisp-mode-symbol-regexp "\\)?")
	     (1 'font-lock-keyword-face)
	     (3 'font-lock-function-name-face nil t)))))
  (islisp-set-fl-keys))



(provide 'easy-islisp)
;;; easy-islisp.el ends here
