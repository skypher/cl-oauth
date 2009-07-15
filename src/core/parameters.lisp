
(in-package :oauth)

(export '(parameter))

(defun parameter-from-http-auth (name &optional (request *request*))
  nil) ; stub; not supported yet.
  
(defun parameter-from-post-data (name &optional (request *request*))
  (hunchentoot:post-parameter name))

(defun parameter-from-get-data (name &optional (request *request*))
  (hunchentoot:get-parameter name))

(defun parameter (name &optional (request *request*))
  "Get the parameter NAME from REQUEST. See section 5.2."
  ;; TODO: check that the parameter appears only once.
  (or (parameter-from-http-auth name request)
      (parameter-from-post-data name request)
      (parameter-from-get-data name request)))

(defun parameters (&optional (request *request*))
  (append nil ; TODO: http auth header parameters
    (hunchentoot:post-parameters request)
    (hunchentoot:get-parameters request)))

(defun normalized-parameters (&key (request *request*)
                                   (auth-parameters-fn (constantly nil))
                                   (post-parameters-fn #'hunchentoot:post-parameters)
                                   (get-parameters-fn #'hunchentoot:get-parameters))
  "9.1.1"
  ;; collect parameters and remove those excluded by the standard.
  (let ((parameters (remove "oauth_signature"
                            (append (remove "realm" (funcall auth-parameters-fn request)
                                            :key #'car :test #'equalp) ; TODO: http auth header parameters
                                    (funcall post-parameters-fn request)
                                    (funcall get-parameters-fn request))
                            :key #'car :test #'equalp)))
    (sort parameters #'string< :key (lambda (x)
                                      "Sort by key and value."
                                      (concatenate 'string (princ-to-string (car x))
                                                           (princ-to-string (cdr x)))))))

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

