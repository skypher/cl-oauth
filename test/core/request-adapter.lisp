
(in-package :oauth-test)

(defvar *request-object* nil)
(defvar *request-method* :get)
(defvar *request-uri* "http://host.domain/path")
(defvar *auth-parameters* nil)
(defvar *post-parameters* nil)
(defvar *get-parameters* nil)

(defun make-test-request-adapter ()
  (make-request-adapter :request-object-fn (lambda ()
                                             ;; prevent caching.
                                             ;; TODO: use private caches for testing
                                             ;; so we don't interfere with live data.
                                             (or *request-object* (random most-positive-fixnum)))
                        :request-method-fn (lambda (request)
                                             (declare (ignore request))
                                             *request-method*)
                        :request-uri-fn (lambda (request)
                                          (declare (ignore request))
                                          *request-uri*)
                        :auth-parameters-fn (lambda (request)
                                              (declare (ignore request))
                                              *auth-parameters*)
                        :post-parameters-fn  (lambda (request)
                                              (declare (ignore request))
                                              *post-parameters*)
                        :get-parameters-fn (lambda (request)
                                              (declare (ignore request))
                                              *get-parameters*)))

(defun init-test-request-adapter ()
  (setf *request-adapter* (make-test-request-adapter)))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system :cl-oauth))))
  (let ((original-request-adapter *request-adapter*))
    (unwind-protect 
        (progn
          (init-test-request-adapter)
          (fiveam:run! 'oauth))
      (setf *request-adapter* original-request-adapter))))

