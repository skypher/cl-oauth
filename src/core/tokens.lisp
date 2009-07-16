
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

(defstruct token
  (key (random-key))
  (secret (random-secret)))

(defstruct (consumer-token (:include token)))
(defstruct (request-token (:include token)))
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
  ;; TODO: appropriate 400/401 return code to simplify debugging
  ;; on the consumer side.
  (let ((parameters (normalized-parameters :request request)))
    (flet ((parameter (key)
              (cdr (assoc key parameters))))
      (unless (member (parameter "oauth_version") '("1.0" nil) :test #'equalp)
        (cerror "Not prepared to handle OAuth version other than 1.0"))
      (unless (equalp (parameter "oauth_signature_method") "HMAC-SHA1")
        (error "Signature method different from HMAC-SHA1"))
      ;; TODO: check timestamp and nonce
      (let ((consumer-key (parameter "oauth_consumer_key")))
        (unless consumer-key
          (error "Consumer key not supplied"))
        (let ((consumer-token (get-consumer-token consumer-key)))
          (unless consumer-token
            (error "Can't identify consumer"))
          ;; TODO: validate signature
          ;; TODO: save callback uri/check oob
          )))))


;;; access token management
(defvar *issued-access-tokens* (make-hash-table :test #'equalp))

(defun validate-authorization-request (&optional (request *request*))
  "Validate an authorization request. Returns either T on
  successful validation or (VALUES NIL <REASON>) on validation
  failure. <REASON> is one of :TOKEN-NOT-PRESENT or :INVALID-TOKEN.
  The Service Provider application may use this return value
  to customize its response (e.g. fail or show a form when
  the token wasn't supplied)."
  (let ((parameters (normalized-parameters :request request)))
    ;; TODO: check if oauth_token is present
    ;; TODO: check oauth_token against our request token database.
    t))

(defun authorization-response (&optional (request *request*))
  "I authorized your token for you. However in real applications you must
  authenticate the user and ask him for authorization. See 6.2.2.")

