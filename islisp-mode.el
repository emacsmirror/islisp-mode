;;; islisp-mode.el ---  Major mode for ISLisp programming.            -*- lexical-binding: t; -*-

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

;;; Code:

;;;; The requires

(require 'font-lock)
(require 'lisp-mode)
(require 'islisp-hyperdraft)

(defgroup islisp nil
  "ISLisp support."
  :group 'languages)

;;TODO: Make this a list.
(defcustom islisp-current-implementation 'easy-islisp
  "Current mode implementation.
Each implementation may add custom functionality,
the supported implementations are define in the implementations
directory."
  :type 'symbol
  :group 'islisp)

(defun islisp-use-implementation ()
  "Initialise the `islisp-current-implementation'."
  (funcall #'require islisp-current-implementation)
  (funcall (intern
	    (apply 'concat `(,(format "%S" islisp-current-implementation) "-init")))))

(defvar islisp-mode-map (make-sparse-keymap))

;;(define-key islisp-mode-map (kbd "C-c C-d") 'islisp-describe-symbol)

(define-key islisp-mode-map (kbd "C-c C-i") 'islisp-repl)
(define-key islisp-mode-map (kbd "C-c M-q") 'indent-region)

;; islisp-mode keybindings
(define-key islisp-mode-map (kbd "M-C-x") 'islisp-eval-defun)
(define-key islisp-mode-map (kbd "C-x C-e") 'islisp-eval-last-sexp)
(define-key islisp-mode-map (kbd "C-c C-r") 'islisp-eval-region)
(define-key islisp-mode-map (kbd "C-c C-l") 'islisp-load-file)
(define-key islisp-mode-map (kbd "C-c C-k") 'islisp-compile-file)
(define-key islisp-mode-map (kbd "C-c C-d") 'islisp-lookup-documentation)

(defun islisp--create-mode-menu ()
  "Internal function to create or recreate the plisp-mode menu."
  (easy-menu-define islisp-menu islisp-mode-map "Menu bar entry for `islisp-mode'"
    '("ISLisp"
      ["Eval last sexp" islisp-eval-last-sexp :keys "C-x C-e"]
      ["Eval function" islisp-eval-defun t  :keys "M-C-x"]
      ["Eval region" islisp-eval-region t  :keys "M-C-x"]
      ["Lookup Documentation" islisp-lookup-documentation t  :keys "C-c C-d"]
      "--"
      ["ISLisp REPL" islisp-repl t :keys "C-c C-i"]
      ["Comment/Uncomment region" comment-line t :keys "C-x C-;"]
      ["Load file" islisp-load-file t  :keys "C-c C-l"]
      ["Compile file" islisp-compile-file t :keys "C-c C-k"]
      "--")))

(defconst islisp-mode-symbol-regexp lisp-mode-symbol-regexp)

(defun islisp-mode-variables ()
  "ISLisp major mode default variables."
  (set-syntax-table lisp-mode-syntax-table)
  (setq-local paragraph-ignore-fill-prefix t
	      fill-paragraph-function 'lisp-fill-paragraph
	      adaptive-fill-function #'lisp-adaptive-fill
	      indent-line-function 'lisp-indent-line
	      indent-region-function 'lisp-indent-region
	      comment-indent-function #'lisp-comment-indent
	      outline-level 'lisp-outline-level
	      add-log-current-defun-function #'lisp-current-defun-name
	      comment-start ";"
	      comment-start-skip ";+ *"
	      comment-add 1
	      comment-column 40
	      comment-use-syntax t
	      multibyte-syntax-as-symbol t
	      completion-ignored-extensions (remove ".o" completion-ignored-extensions))
  (islisp-set-fl-keys))

(defun islisp-set-fl-keys ()
  (setq-local
   font-lock-defaults `(islisp-font-lock-keywords
		nil nil nil nil
		(font-lock-syntactic-face-function
		 . lisp-font-lock-syntactic-face-function))))

(defvar islisp-general-keywords
  '("-" "*" "/=" "+" "<" "<=" "=" ">" ">="
    "abs" "append" "apply" "aref" "arithmetic-error-operands"
    "arithmetic-error-operation" "array-dimensions" "assoc" "atan"
    "atan2" "atanh" "atom" "basic-array-p" "basic-array*-p"
    "basic-vector-p" "call-next-method" "car" "cdr" "ceiling"
    "char-index" "char/=" "char<" "char<=" "char="
    "char>" "char>=" "characterp" "class-of" "close"
    "condition-continuable" "cons" "consp" "continue-condition"
    "cos" "cosh" "create-array" "create-list"
    "create-string-input-stream"
    "create-string-output-stream" "create-string" "create-vector"
    "create"
    "div" "domain-error-object" "domain-error-expected-class"
    "dummyp" "elt" "eq" "eql" "equal" "error-output" "error"
    "eval" "exp" "expt" "file-length" "file-position" "finish-output"
    "float" "floatp" "floor" "format-char" "format-fresh-line"
    "format-float" "format-integer" "format-object" "format-tab"
    "format"
    "funcall" "functionp" "garef" "gbc" "gcd" "general-array*-p"
    "general-vector-p" "generic-function-p" "gensym"
    "get-internal-real-time"
    "get-internal-run-time"
    "get-output-stream-string" "get-universal-time" "hdmp" "identity"
    "initialize-object" "input-stream-p" "instancep" "integerp"
    "internal-time-units-per-second" "isqrt" "lcm" "length" "list"
    "listp" "load" "log" "map-into" "mapc" "mapcar" "mapcan"
    "mapcon" "mapl" "maplist" "max" "member" "min" "mod"
    "next-method-p" "not" "nreverse" "null" "numberp"
    "open-input-file" "open-io-file" "open-output-file" "open-stream-p"
    "output-stream-p" "parse-error-string" "parse-error-expected-class"
    "parse-number" "preview-char" "prin1" "print" "probe-file"
    "property" "quit" "quotient" "read-byte" "read-char" "read-line"
    "read" "reciprocal" "remove-property" "reverse" "round"
    "set-aref"
    "set-car" "set-cdr" "set-elt" "set-file-position" "set-garef"
    "set-property" "signal-condition" "simple-error-format-argument"
    "simple-error-format-string" "sin" "sinh" "slot-value" "sqrt"
    "standard-input" "standard-output" "stream-error-stream" "streamp"
    "stream-ready-p" "string-append" "string-index" "string/="
    "string<" "string<=" "string=" "string>" "string>=" "stringp"
    "subclassp"
    "subseq" "symbolp" "tan" "tanh" "truncate"
    "undefined-entity-name"
    "undefined-entity-namespace" "vector" "write-byte" "import"
    "lambda" "labels" "flet" "let" "let*" "setq" "setf"
    "dynamic" "set-dynamic" "function" "function*" "symbol-function" "class"
    "and" "or" "if" "cond" "while" "for" "block" "return-from"
    "case" "case-using" "progn" "dynamic-let" "ignore-errors" "catch" "throw"
    "tagbody" "go" "unwind-protect" "with-standard-input"
    "with-standard-output" "with-error-output" "with-handler"
    "convert" "with-open-input-file" "with-open-output-file"
    "with-open-io-file" "the" "assure"))

(defvar islisp-font-lock-keywords-regex
  (regexp-opt islisp-general-keywords t))

(defvar islisp-def-keywords
  '("defconstant" "defglobal" "defdynamic"))

(defvar islisp-def-keywords-regex
  (regexp-opt islisp-def-keywords t))

(defvar islisp-func-def-keywords
  '("defun" "defmethod" "defmacro" "defgeneric" "defclass" "defgeneric*"))

(defvar islisp-func-def-keywords-regex
  (regexp-opt islisp-func-def-keywords t))

(defvar islisp-font-lock-keywords
  `((,(concat "(" islisp-font-lock-keywords-regex "\\_>")
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
     (3 'font-lock-function-name-face nil t))))

(defun islisp-repl()
  "Start a ISLisp REPL or switch to it."
  (interactive)
  (require 'inferior-islisp)
  (if (and (stringp inferior-islisp-buffer)
	   (get-buffer inferior-islisp-buffer))
      (pop-to-buffer inferior-islisp-buffer)
    (inferior-islisp)))

;;;###autoload
(define-derived-mode islisp-mode prog-mode "ISLisp"
  "Major mode for editing ISLisp code"
  (islisp-mode-variables)
  (setq-local
   find-tag-default-function 'lisp-find-tag-default
   comment-start-skip
   "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(;+\\|#|\\) *")
  (islisp--create-mode-menu)
  (islisp-use-implementation))

(advice-add 'islisp-mode :after #'(lambda ()
			      (setq-local font-lock-keywords-case-fold-search t)))

(add-to-list 'auto-mode-alist '("\\.lsp\\'" . islisp-mode))


(provide 'islisp-mode)
;;; islisp-mode.el ends here
