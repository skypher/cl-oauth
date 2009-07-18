(in-package :oauth)

;;; server-specific request abstraction layer
;;;
;;; defaults to Hunchentoot

(export '(request-adapter
          make-request-adapter
          *request-adapter*
          make-hunchentoot-request-adapter
          *request*
          request
          request-method
          post-parameters
          get-parameters))


#.`(defstruct request-adapter
     ,@(loop for slotname in '(request-object-fn request-method-fn
                               auth-parameters-fn post-parameters-fn
                               get-parameters-fn)
           collect `(,slotname nil :type (or function null)))
     (:documentation "An adapter for server-specific parts of OAuth.
     The return value of REQUEST-OBJECT-FN must be comparable with EQ."))

(defun make-hunchentoot-request-adapter ()
  (make-request-adapter :request-object-fn (lambda () hunchentoot:*request*)
                        :request-method-fn #'hunchentoot:request-method*
                        :auth-parameters-fn (lambda (request)
                                              nil) ; TODO
                        :post-parameters-fn #'hunchentoot:post-parameters*
                        :get-parameters-fn #'hunchentoot:get-parameters*))
                                              

(defvar *request-adapter* (make-hunchentoot-request-adapter)
  "Set this variable to an instance of REQUEST-ADAPTER tailored to
  your web server.")

(defvar *request* nil
  "User-supplied request override. Only if you're know what you're doing.")

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

;; TODO: assertions/type checks for the following functions
  
(defun auth-parameters (&optional (request (request)))
  (funcall (request-adapter-auth-parameters-fn *request-adapter*) request))

(defun post-parameters (&optional (request (request)))
  (funcall (request-adapter-post-parameters-fn *request-adapter*) request))

(defun get-parameters (&optional (request (request)))
  (funcall (request-adapter-get-parameters-fn *request-adapter*) request))

