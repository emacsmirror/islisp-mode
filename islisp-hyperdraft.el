;;; islisp-hyperdraft.el ---  islisp-mode hyperdraft interaction       -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Fermin Munoz

;; Author: Fermin Munoz
;; Maintainer: Fermin Munoz <fmfs@posteo.net>
;; Created: 24 Sep 2021
;; Version: 0.1.0
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
(require 'eww)


(defcustom islisp-hyperdraft-root "https://nenbutsu.github.io/ISLispHyperDraft/islisp-v23.html"
  "The root of ISLisp HyperDraft URL.
If you copy the Hyperdraft to your local system, set this variable to
something like \"file:/usr/local/doc/HyperDraft/islisp-v23.html\"."
  :type 'url
  :group 'islisp)

(defvar islisp-hyperdraft-symbols
  '(("#'" . "#s_function") ("#|" . "#comments") ("'" . "#s_quote")
    ("*" . "#f_muptiply") ("*most-negative-float*" . "#c_most_negative_float")
    ("*most-positive-float*" . "#c_most_positive_float") ("*pi*" . "#c_pi")
    ("+" . "#f_plus") ("," . "#variables") (",@" . "#syn_comma_at")
    ("-" . "#f_minus") ("/=" . "#f_math_neq") ("<" . "#f_lt") ("<=" . "#f_lteq")
    ("=" . "#f_math_eq") (">" . "#f_gt") (">=" . "#f_gteq")
    ("`" . "#syn_backquote") ("|#" . "#comments") ("abs" . "#f_abs")
    ("and" . "#s_and") ("append" . "#f_append") ("apply" . "#f_apply")
    ("aref" . "#f_aref") 
    ("arithmetic-error-operands" . "#f_arithmetic_error_operands")
    ("arithmetic-error-operation" . "#f_arithmetic_error_operation")
    ("array-dimensions" . "#f_array_dimensions") ("assignment" . "#variables")
    ("assoc" . "#f_assoc") ("assure" . "#s_assure") ("atan" . "#f_atan")
    ("atan2" . "#f_atan2") ("atanh" . "#f_atanh")
    ("basic-array-p" . "#f_basic_array_p")
    ("basic-vector-p" . "#f_basic_vector_p") ("binary i/o" . "#binary_io")
    ("binding" . "#td_binding") ("block" . "#s_block")
    ("call-next-method" . "#l_call_next_method") ("car" . "#f_car")
    ("case" . "#s_case") ("case-using" . "#s_case_using") ("catch" . "#s_catch")
    ("cdr" . "#f_cdr") ("ceiling" . "#f_ceiling")
    ("cerror" . "#f_cerror") ("char-index" . "#f_char_index")
    ("char/=" . "#f_char_neq") ("char<" . "#f_char_lt")
    ("char<=" . "#f_char_lteq") ("char=" . "#f_char_eq") ("char>" . "#f_char_gt")
    ("char>=" . "#f_char_gteq") ("character" . "#character")
    ("characterp" . "#f_characterp")
    ("class" . "#s_class") ("class-of" . "#f_class_of") ("close" . "#f_close")
    ("coercion" . "#s_convert") ("cond" . "#s_cond") ("cons" . "#f_cons")
    ("consp" . "#f_consp") ("continue-condition" . "#f_continue_condition")
    ("convert" . "#s_convert") ("cos" . "#f_cos") ("cosh" . "#f_cosh")
    ("create" . "#gf_create") ("create-array" . "#f_create_array")
    ("create-list" . "#f_create_list") ("create-string" . "#f_create_string")
    ("create-string-input-stream" . "#f_create_string_input_stream")
    ("create-string-output-stream" . "#f_create_string_output_stream")
    ("create-vector" . "#f_create_vector") ("defclass" . "#def_defclass")
    ("defconstant" . "#def_defconstant") ("defdynamic" . "#def_defdynamic")
    ("defgeneric" . "#def_defgeneric") ("defglobal" . "#def_defglobal")
    ("defining form" . "#defining_forms")
    ("defining operator" . "#defining_forms") ("defining-form" . "#def_")
    ("defining-form-name" . "#defining_forms")
    ("definition point" . "#td_definition_point") ("defmacro" . "#def_defmacro")
    ("defmethod" . "#def_defmethod") ("defun" . "#def_defun")
    ("destination" . "#non_local_exits") ("div" . "#f_div")
    ("domain-error-expected-class" . "#f_domain_error_expected_class")
    ("domain-error-object" . "#f_domain_error_object") ("dynamic" . "#s_dynamic")
    ("dynamic-let" . "#s_dynamic_let") ("elt" . "#f_elt") ("eq" . "#f_eq")
    ("eql" . "#f_eql") ("equal" . "#f_equal") ("error" . "#f_error")
    ("error-output" . "#f_error_output") ("exp" . "#f_exp")
    ("expander" . "#macros") ("expt" . "#f_expt") ("extension" . "#td_extension")
    ("extent" . "#extent") ("file-length" . "#f_file_length")
    ("file-position" . "#f_file_position") ("finish-output" . "#f_finish_output")
    ("flet" . "#s_flet") ("ﬂoat" . "#float11") ("float" . "#f_float")
    ("floatp" . "#f_floatp") ("floor" . "#f_floor") ("for" . "#s_for")
    ("format" . "#f_format") ("format-char" . "#f_format_char")
    ("format-float" . "#f_format_float")
    ("format-fresh-line" . "#f_format_fresh_line")
    ("format-integer" . "#f_format_integer")
    ("format-object" . "#f_format_object") ("format-tab" . "#f_format_tab")
    ("funcall" . "#f_funcall") ("function-name" . "#function_application_forms")
    ("functionp" . "#f_functionp") ("garef" . "#f_garef") ("gcd" . "#f_gcd")
    ("generic-function-p" . "#f_generic_function_p") ("gensym" . "#f_gensym")
    ("get-internal-real-time" . "#f_get_internal_real_time")
    ("get-internal-run-time" . "#f_get_internal_run_time")
    ("get-output-stream-string" . "#f_create_string_output_stream")
    ("get-universal-time" . "#f_get_universal_time") ("go" . "#s_go")
    ("identity" . "#f_identity") ("if" . "#s_if")
    ("ignore-errors" . "#s_ignore_errors")
    ("initialize-object" . "#gf_initialize_object")
    ("input-stream-p" . "#f_input_stream_p") ("instance" . "#td_instance")
    ("integer" . "#integer_class") ("integerp" . "#f_integerp")
    ("internal-time-units-per-second" . "#f_internal_time_units_per_second")
    ("isqrt" . "#f_isqrt") ("labels" . "#s_labels") ("lambda" . "#s_lambda")
    ("lcm" . "#f_lcm") ("length" . "#f_length") ("let" . "#s_let")
    ("let*" . "#s_let_s") ("list" . "#f_list") ("listp" . "#f_listp")
    ("literal" . "#td_literal") ("log" . "#f_log") ("map-into" . "#f_map_into")
    ("mapc" . "#f_mapc") ("mapcan" . "#f_mapcan") ("mapcar" . "#f_mapcar")
    ("mapcon" . "#f_mapcon") ("mapl" . "#f_mapl") ("maplist" . "#f_maplist")
    ("max" . "#f_max") ("member" . "#f_member") ("min" . "#f_min")
    ("mod" . "#f_mod") ("next-method-p" . "#l_next_method_p") ("nil" . "#c_nil")
    ("not" . "#f_not") ("nreverse" . "#f_nreverse") ("null" . "#f_null")
    ("numberp" . "#f_numberp") ("open-input-file" . "#f_open_input_file")
    ("open-io-file" . "#f_open_io_file")
    ("open-output-file" . "#f_open_output_file")
    ("open-stream-p" . "#f_open_stream_p") ("operator" . "#td_operator")
    ("or" . "#s_or") ("output-stream-p" . "#f_output_stream_p") ("pair" . "#cons")
    ("parse-error-expected-class" . "#f_parse_error_expected_class")
    ("parse-error-string" . "#f_parse_error_string")
    ("parse-number" . "#f_parse_number") ("place" . "#td_place")
    ("preview-char" . "#f_preview_char")
    ("primary method" . "#simple_method_combination")
    ("probe-file" . "#f_probe_file") ("process" . "#td_process")
    ("progn" . "#s_progn") ("program" . "#td_program")
    ("property" . "#f_property") ("quote" . "#s_quote")
    ("quotient" . "#f_quotient") ("read" . "#f_read")
    ("read-byte" . "#f_read_byte") ("read-char" . "#f_read_char")
    ("read-line" . "#f_read_line") ("reciprocal" . "#f_reciprocal")
    ("remove-property" . "#f_remove_property")
    ("report-condition" . "#gf_report_condition")
    ("return-from" . "#s_return_from") ("reverse" . "#f_reverse")
    ("round" . "#f_round") ("set-aref" . "#f_set_aref") ("set-car" . "#f_set_car")
    ("set-cdr" . "#f_set_cdr") ("set-dynamic" . "#s_set_dynamic")
    ("set-elt" . "#f_set_elt") ("set-file-position" . "#f_set_file_position")
    ("set-garef" . "#f_set_garef") ("set-property" . "#f_set_property")
    ("set-up forms" . "#toplevel_form") ("setf" . "#s_setf") ("setq" . "#s_setq")
    ("signal-condition" . "#f_signal_condition")
    ("simple-error-format-arguments" . "#f_simple_error_format_arguments")
    ("simple-error-format-string" . "#f_simple_error_format_string")
    ("sin" . "#f_sin") ("sinh" . "#f_sinh") ("slot" . "#td_slot")
    ("sqrt" . "#f_sqrt") ("standard-input" . "#f_standard_input")
    ("standard-output" . "#f_standard_output")
    ("stream-error-stream" . "#f_stream_error_stream")
    ("stream-ready-p" . "#f_stream_ready_p") ("streamp" . "#f_streamp")
    ("string streams" . "#other_streams") ("string-append" . "#f_string_append")
    ("string-index" . "#f_string_index") ("string/=" . "#f_stringneq")
    ("string<" . "#f_stringlt") ("string<=" . "#f_stringlteq")
    ("string=" . "#f_stringeq") ("string>" . "#f_stringgt")
    ("string>=" . "#f_stringgteq") ("stringp" . "#f_stringp")
    ("subclassp" . "#f_subclassp") ("subseq" . "#f_subseq")
    ("stream" . "#stream_class") ("symbolp" . "#f_symbolp") ("t" . "#c_t")
    ("tagbody" . "#s_tagbody") ("tan" . "#f_tan") ("tanh" . "#f_tanh")
    ("the" . "#s_the") ("throw" . "#s_throw") ("truncate" . "#f_truncate")
    ("undefined-entity-name" . "#f_undefined_entity_name")
    ("undefined-entity-namespace" . "#f_undefined_entity_namespace")
    ("unwind-protect" . "#s_unwind_protect") ("vector" . "#f_vector")
    ("while" . "#s_while") ("with-error-output" . "#s_with_error_output")
    ("with-handler" . "#s_with_handler")
    ("with-open-input-file" . "#s_with_open_input_file")
    ("with-open-io-file" . "#s_with_open_io_file")
    ("with-open-output-file" . "#s_with_open_output_file")
    ("with-standard-input" . "#s_with_standard_input")
    ("with-standard-output" . "#s_with_standard_output")
    ("write-byte" . "#f_write_byte") ("writer" . "#td_writer")))

(defun islisp-lookup-documentation ()
  (interactive)
  (let* ((csymbol (thing-at-point 'symbol t))
	 (current-symbol (assoc csymbol  islisp-hyperdraft-symbols)))
    (unless current-symbol
      (setf current-symbol
	    (assoc (completing-read
		    "ISLisp symbol: "
		    (mapcar #'car islisp-hyperdraft-symbols)
		    nil t csymbol)
		   islisp-hyperdraft-symbols)))
    (eww (concat islisp-hyperdraft-root (cdr current-symbol)))))


(provide 'islisp-hyperdraft)
;;; islisp-hyperdraft.el ends here
