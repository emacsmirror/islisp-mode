(import "formatter")


(defglobal tmp-directory "/tmp")
(defglobal string-location-file "/tmp/easy-islisp-formatter331.lsp")

(defun end-of-file-p (x)
  (eq x 'eof))

(defun unique-file-name (&rest intents)
  (if (not intents) (setf intents 0) (setf intents (car intents)))
  (let* ((letters #("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k"))
	 (rname (for ((word "")
		      (i 0 (+ i 1)))
		     ((= i 5) word)
		     (setf word (string-append
				 word
				 (aref letters (random 10))))))
	 (file (string-append rname (convert (random 50000) <string>))))
    (if (>= intents 500)
	(error "Error creating temporal file name.")
      (if (probe-file (string-append tmp-directory "/" file))
	  (unique-file-name (+ intents 1))
	file))))

(defun get-file-content (file add-new-line)
  (let* ((temp-stream nil)
	 (output "")
	 (temp-line nil)
	 (output-stream (create-string-output-stream)))
    (setf temp-stream (open-input-file file))
    (setf temp-line (read-line temp-stream nil 'eof))
    (while (not (end-of-file-p temp-line))
      (format output-stream temp-line)
      (if add-new-line (format output-stream "\\n"))
      (setf temp-line (read-line temp-stream nil 'eof)))
    (close temp-stream)
    (format output-stream output)
    (get-output-stream-string output-stream)))

(defun string-formatter (string)
  (let* ((temp-name (unique-file-name))
	 (temp-file (string-append tmp-directory "/" temp-name ".lsptmp"))
	 (temp-file-org (string-append tmp-directory "/" temp-name ".org"))
	 (temp-file-content nil))
    ;; Write in the tmp file
    (with-open-output-file (out temp-file)
			   (format out string))
    (formatter temp-file)
    (setf temp-file-content (get-file-content temp-file t))

    (if (probe-file temp-file)
	(system (string-append "rm " temp-file)))

    (if (probe-file temp-file-org)
	(system (string-append "rm " temp-file-org)))
    temp-file-content))

(defun easy-islisp-format-buffer ()
  (if (probe-file string-location-file)
      (progn
	(formatter string-location-file)
	(format (standard-output) (get-file-content string-location-file nil)))
    (error "Easy-islisp file not found.")))

(easy-islisp-format-buffer)



