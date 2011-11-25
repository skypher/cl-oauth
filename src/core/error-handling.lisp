
(in-package :oauth)


(define-condition http-error (error)
  ((status-code :reader http-error-status-code
                :initarg :status-code)
   (reason-phrase :reader http-error-reason-phrase
                  :initarg :reason-phrase)))

(define-condition bad-request (http-error)
  () (:default-initargs :status-code 400 :reason-phrase "Bad Request"))

(define-condition unauthorized (http-error)
  () (:default-initargs :status-code 401 :reason-phrase "Unauthorized"))

(defun raise-error (type &optional reason-phrase-fmt &rest reason-phrase-args)
  (if reason-phrase-fmt
    (let ((reason-phrase (apply #'format nil reason-phrase-fmt reason-phrase-args)))
      (error type :reason-phrase reason-phrase))
    (error type)))

(defun default-error-handler (condition)
  "Default error handler for conditions of type HTTP-ERROR."
  (check-type condition http-error)
  (let ((status-code (http-error-status-code condition))
        (reason-phrase (http-error-reason-phrase condition)))
    (setf (hunchentoot:return-code*) status-code)
    (setf (hunchentoot:content-type*) "text/plain")
    (abort-request
      (format nil "~D ~A" status-code reason-phrase))))

(defmacro protocol-assert (&body body)
  `(unless (progn ,@body)
     (raise-error 'bad-request "Failed protocol assertion")))

