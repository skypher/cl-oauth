
(asdf:oos 'asdf:load-op 'cl-oauth)
(asdf:oos 'asdf:load-op 'hunchentoot)

(defpackage :cl-oauth.yahoo-consumer
  (:use :cl :cl-oauth))

(in-package :cl-oauth.yahoo-consumer)

;;; insert your credentials and auxiliary information here.
(defparameter *key* "")
(defparameter *secret* "") 
(defparameter *callback-uri* "")
;(defparameter *additional-parameters* '(("AppId" . "")))
(defparameter *callback-port* 8090
  "Port to listen on for the callback")



;;; go
(defparameter *get-request-token-endpoint* "https://api.login.yahoo.com/oauth/v2/get_request_token")
(defparameter *auth-request-token-endpoint* "https://api.login.yahoo.com/oauth/v2/request_auth")
(defparameter *get-access-token-endpoint* "https://api.login.yahoo.com/oauth/v2/get_token")
(defparameter *consumer-token* (make-consumer-token :key *key* :secret *secret*))
(defparameter *request-token* nil)
(defparameter *access-token* nil)

(defun get-access-token ()
  (obtain-access-token *get-access-token-endpoint* *request-token*))

;;; get a request token
(defun get-request-token ()
  (obtain-request-token
    *get-request-token-endpoint*
    *consumer-token*
    :callback-uri *callback-uri*))

(setf *request-token* (get-request-token))

(let ((auth-uri (make-authorization-uri *auth-request-token-endpoint* *request-token*)))
  (format t "Please authorize the request token at this URI: ~A~%" (puri:uri auth-uri)))


;;; set up callback uri
(defun callback-dispatcher (request)
  (declare (ignorable request))
  (unless (cl-ppcre:scan  "favicon\.ico$" (hunchentoot:script-name request))
    (lambda (&rest args)
      (declare (ignore args))
      (handler-case
          (authorize-request-token-from-request
            (lambda (rt-key)
              (assert *request-token*)
              (unless (equal (url-encode rt-key) (token-key *request-token*))
                (warn "Keys differ: ~S / ~S~%" (url-encode rt-key) (token-key *request-token*)))
              *request-token*))
        (error (c)
          (warn "Couldn't verify request token authorization: ~A" c)))
      (when (request-token-authorized-p *request-token*)
        (format t "Successfully verified request token with key ~S~%" (token-key *request-token*))
        (setf *access-token* (get-access-token))
        (let ((reply-body (access-protected-resource
                            "http://social.yahooapis.com/v1/user/jupitercollision/profile"
                            *access-token*
                            ;; Yahoo uses OAuth session so the token might need refresh.
                            :on-refresh (lambda (new-token)
                                          (setf *access-token* new-token)) )))
          (etypecase reply-body
            (string reply-body)
            ((vector (unsigned-byte 8)) (babel:octets-to-string reply-body))))))))

(pushnew 'callback-dispatcher hunchentoot:*dispatch-table*)


(defvar *web-server* nil)

(when *web-server*
  (hunchentoot:stop *web-server*)
  (setf *web-server* nil))

(setf *web-server* (hunchentoot:start (make-instance 'hunchentoot:acceptor :port *callback-port*)))

