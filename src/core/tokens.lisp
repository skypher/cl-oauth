
(in-package :oauth)

;;; section 6.
;;;
;;;  OAuth Authentication is done in three steps:
;;;
;;;    1. The Consumer obtains an unauthorized Request Token.
;;;    2. The User authorizes the Request Token.
;;;    3. The Consumer exchanges the Request Token for an Access Token.
;;;

;;; TODO: token GC

;;; tokens
(defun random-key ()
  "key")

(defun random-secret ()
  "secret")

(defun random-verification-code ()
  "verification_code")

(defun finalize-callback-uri (request-token)
  (let ((uri (request-token-callback-uri request-token)))
    (setf (puri:uri-query uri)
          (concatenate 'string (or (puri:uri-query uri) "")
                       (if (puri:uri-query uri) "&" "")
                       "oauth_token="
                         (url-encode (request-token-key request-token))
                       "&oauth_verifier="
                         (url-encode (request-token-verification-code request-token))))
    uri))

(defstruct token
  (key (random-key) :type string)
  (secret (random-secret) :type string))

(defstruct (consumer-token (:include token)))

(defstruct (request-token (:include token))
  (callback-uri nil :type (or null puri:uri))
  (verification-code (random-verification-code) :type string)
  (authorized-p nil :type boolean))

(defstruct (access-token (:include token)))

;;; consumer management
(defvar *registered-consumers* (make-hash-table :test #'equalp))

(defun register-consumer (&optional (token (make-consumer-token)))
  "Register a consumer. TOKEN-ARGS will be relayed to the token constructor.
  Returns the consumer's token."
  (setf (gethash (token-key token) *registered-consumers*) token)
  token)

(defun get-consumer-token (key)
  (gethash key *registered-consumers*))


;;; request token management
(defvar *issued-request-tokens* (make-hash-table :test #'equalp))

(defun request-token-response (token &rest additional-parameters)
  (declare (ignore additional-parameters)) ; not supported
  (url-encode (alist->query-string
                `(("oauth_token" . ,(token-key token))
                  ("oauth_token_secret" . ,(token-secret token))
                  ("oauth_callback_confirmed" . "true")))))

(defun validate-request-token-request (&optional (request *request*))
  "Check whether REQUEST is a valid request token request.
  
  Returns the supplied Consumer callback (a PURI:URI) or NIL if
  the callback is supposed to be transferred oob. [6.1.1]"
  ;; TODO: appropriate 400/401 return code to simplify debugging
  ;; on the consumer side.
  ;;
  ;; TODO: raise specific errors instead of simple errors
  ;;
  ;; TODO: maybe return a newly created request token here?
  (let ((parameters (normalized-parameters :request request)))
    (flet ((parameter (key)
              (cdr (assoc key parameters))))
      (let ((version (parameter "oauth_version")))
        (unless (member version '("1.0" nil) :test #'equalp)
          (cerror "Not prepared to handle OAuth version other than 1.0" version)))
      (unless (equalp (parameter "oauth_signature_method") "HMAC-SHA1")
        (error "Signature method different from HMAC-SHA1"))
      ;; TODO: check timestamp and nonce
      (let ((consumer-key (parameter "oauth_consumer_key")))
        (unless consumer-key
          (error "Consumer key not supplied"))
        (let ((consumer-token (get-consumer-token consumer-key))
              (callback (parameter "oauth_callback")))
          (unless consumer-token
            (error "Can't identify consumer"))
          (unless callback
            (error "No callback supplied"))
          ;; TODO: validate signature
          (unless (equalp callback "oob")
            (puri:parse-uri callback)))))))


;;; access token management
(defvar *issued-access-tokens* (make-hash-table :test #'equalp))

(defun validate-authorization-request (&optional (request *request*))
  "Validate an authorization request. Returns the request token object
  on successful validation or (VALUES NIL <REASON>) on validation
  failure. <REASON> is one of :TOKEN-NOT-PRESENT or :INVALID-TOKEN.
  The Service Provider application may use this return value
  to customize its response (e.g. fail or show a form when
  the token wasn't supplied)."
  ;; TODO: raise specific errors
  ;; TODO: 6.2.1 says that this request comes in via GET; verify this
  (let ((parameters (normalized-parameters :request request)))
    (flet ((parameter (key)
              (cdr (assoc key parameters))))
      (let ((request-token-key (parameter "oauth_token")))
        ;; check if the Consumer supplied a request token
        (unless request-token-key
          (error "No request token identifier (oauth_token) supplied."))
        ;; check if the supplied request token is known to us
        (let ((request-token (gethash request-token-key *issued-request-tokens*)))
          (unless request-token
            (error "Invalid request token."))
          request-token)))))

