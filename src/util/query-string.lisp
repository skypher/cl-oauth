
(in-package :oauth)

(defun splice-alist (alist)
  (reduce #'nconc (mapcar (lambda (x)
                            (list (car x) (cdr x)))
                          alist)))

(defun alist->query-string (alist &key (include-leading-ampersand t))
    (let ((result (format nil "~{&~A=~A~}" (splice-alist alist))))
      (subseq
        result
        (if (or (zerop (length result)) include-leading-ampersand)
          0
          1))))

(defun query-string->alist (query-string)
  (let* ((kv-pairs (remove "" (split-sequence #\& query-string) :test #'equal))
         (alist (mapcar (lambda (kv-pair)
                          (let ((kv (split-sequence #\= kv-pair)))
                            (cons (first kv) (second kv))))
                        kv-pairs)))
    alist))

