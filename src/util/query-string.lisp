
(in-package :oauth)

(defun alist->query-string (alist &key (include-leading-ampersand t) url-encode)
  (let* ((plist (splice-alist alist))
	 (plist* (if url-encode
                     (loop for (key value) on plist by #'cddr
                           collect (url-encode (string key))
                           collect (url-encode value))
                     plist))
	 (result (format nil "~{&~A=~A~}" plist*)))
    (subseq ; TODO: nsubseq http://darcs.informatimago.com/lisp/common-lisp/utility.lisp
      result
      (if (or (zerop (length result)) include-leading-ampersand)
        0
        1))))

(defun query-string->alist (query-string)
  ;; TODO: doesn't handle leading ?
  (check-type query-string string)
  (let* ((kv-pairs (remove "" (split-sequence #\& query-string) :test #'equal))
         (alist (mapcar (lambda (kv-pair)
                          (let ((kv (split-sequence #\= kv-pair)))
                            (cons (first kv) (second kv))))
                        kv-pairs)))
    alist))

