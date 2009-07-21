
(in-package :oauth)

(defun alist->plist (alist)
  "Converts an alist to plist."
  (let ((keyword-package (find-package :keyword)))
    (loop for i in alist
       collect (if (symbolp (car i))
                   (intern (symbol-name (car i)) keyword-package)
                   (intern (string-upcase (car i)) keyword-package))
       collect (cdr i))))

(defun splice-alist (alist)
  (reduce #'nconc (mapcar (lambda (x)
                            (list (car x) (cdr x)))
                          alist)))

