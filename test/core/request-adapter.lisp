
(in-package :oauth-test)

(defvar *request-method* :get)
(defvar *auth-parameters* nil)
(defvar *post-parameters* nil)
(defvar *get-parameters* nil)

(defun make-test-request-adapter ()
  (make-request-adapter :request-object-fn (lambda ()
                                             ;; prevent caching.
                                             ;; TODO: use private caches for testing
                                             ;; so we don't interfere with live data.
                                             (random 1.0))
                        :request-method-fn (lambda (request)
                                             (declare (ignore request))
                                             *request-method*)
                        :auth-parameters-fn (lambda (request)
                                              (declare (ignore request))
                                              *auth-parameters*)
                        :post-parameters-fn  (lambda (request)
                                              (declare (ignore request))
                                              *post-parameters*)
                        :get-parameters-fn (lambda (request)
                                              (declare (ignore request))
                                              *get-parameters*)))

;; TODO: bind this dynamically to not mess up a live image through testing.
(setf *request-adapter* (make-test-request-adapter))

