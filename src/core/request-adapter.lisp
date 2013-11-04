(in-package :oauth)

;;; server-specific request abstraction layer
;;;
;;; defaults to Hunchentoot

(export '(request-adapter
          make-request-adapter
          *request-adapter*
          make-hunchentoot-request-adapter
          init-default-request-adapter
          *request*
          request
          request-method
          request-uri
          abort-request
          auth-parameters
          post-parameters
          get-parameters)) ; TODO move to package.lisp


#.`(defstruct request-adapter ; TODO: make this a standard-class, too
     "An adapter for server-specific parts of OAuth.
The return value of REQUEST-OBJECT-FN must be comparable with EQ."
     ,@(loop for slotname in '(request-object-fn
                               request-method-fn
                               request-uri-fn
                               abort-request-fn
                               auth-parameters-fn
                               post-parameters-fn
                               get-parameters-fn)
           collect `(,slotname nil :type (or function symbol null))))

(defun make-hunchentoot-request-adapter ()
  (make-request-adapter :request-object-fn (lambda () hunchentoot:*request*)
                        :request-uri-fn (lambda (request)
                                          (let* ((http-host (split-sequence #\: (hunchentoot:host request)))
                                                 (hostname (first http-host))
                                                 (port (second http-host)))
                                            (make-instance 'puri:uri
                                                           :scheme (etypecase hunchentoot:*acceptor*
                                                                     (hunchentoot:ssl-acceptor :https)
                                                                     (hunchentoot:acceptor :http))
                                                           :host hostname
                                                           :port port
                                                           :path (hunchentoot:script-name* request))))
                        :request-method-fn 'hunchentoot:request-method*
                        :abort-request-fn 'hunchentoot:abort-request-handler
                        :auth-parameters-fn (lambda (request) (declare (ignore request)) nil) ; TODO
                        :post-parameters-fn 'hunchentoot:post-parameters*
                        :get-parameters-fn 'hunchentoot:get-parameters*))
                                              

(defvar *request-adapter* nil
  "Set this variable to an instance of REQUEST-ADAPTER tailored to
  your web server.")

(defun init-default-request-adapter ()
  (setf *request-adapter* (make-hunchentoot-request-adapter)))

(init-default-request-adapter)

(defvar *request* nil
  "User-supplied request override. Only if you know what you're doing.")

(defun request ()
  (or *request* ; allow request object override
      (funcall (request-adapter-request-object-fn *request-adapter*))))

(defun request-method (&optional (request (request)))
  (let* ((result (funcall (request-adapter-request-method-fn *request-adapter*) request))
         (normalized-result (etypecase result
                              (keyword result)
                              (symbol (intern (symbol-name result) :keyword))
                              (string (intern result :keyword)))))
    (assert (member normalized-result '(:get :post :put :delete :head :trace :options :connect)))
    result))

(defun request-uri (&optional (request (request)))
  "Return the request uri including protocol, host, port
and path. Other parts like the query string are optional and
will be ignored. The result type is (or string puri:uri)."
  ;; TODO: cache this
  (let ((result (funcall (request-adapter-request-uri-fn *request-adapter*) request)))
    (check-type result (or string puri:uri))
    result))

;; TODO: assertions/type checks for the following functions
  
(defun auth-parameters (&optional (request (request)))
  (funcall (request-adapter-auth-parameters-fn *request-adapter*) request))

(defun post-parameters (&optional (request (request)))
  (funcall (request-adapter-post-parameters-fn *request-adapter*) request))

(defun get-parameters (&optional (request (request)))
  (funcall (request-adapter-get-parameters-fn *request-adapter*) request))


(defun abort-request (result)
  "Return the string RESULT immediately from the request handler."
  (funcall (request-adapter-abort-request-fn *request-adapter*) result))

