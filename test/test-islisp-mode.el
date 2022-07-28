;;; test-islisp-mode.el --- islisp-mode test file.               -*- lexical-binding: t; -*-

;; Copyright (C) 20 Fermin Munoz

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

;;; Code:

;;;; The requires
(require 'ert)
(require 'cl-lib)
(require 'islisp-mode)
(require 'inferior-islisp)

(ert-deftest islisp-inferior-launch-test ()
  "Make sure that the inferior buffer is created"
  (islisp-repl)
  (should (bufferp (get-buffer inferior-islisp-buffer))))

(ert t)





(provide 'test-islisp-mode.el)
;;; test-islisp-mode.el ends here
