
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
  (authorized-p nil :type boolean)
  (consumer nil :type (or consumer-token null)))

(defstruct (access-token (:include token))
  (consumer nil :type (or consumer-token null)))

;; TODO: need to store application-specific data somewhere.

;;; consumer management
(defvar *registered-consumers* (make-hash-table :test #'equalp))

(defun register-consumer (&optional (token (make-consumer-token)))
  "Register a consumer. TOKEN-ARGS will be relayed to the token constructor.
  Returns the consumer's token."
  (setf (gethash (token-key token) *registered-consumers*) token)
  token)

(defun get-consumer-token (key)
  (gethash key *registered-consumers*))


;;; signature checking
(defun check-signature ()
  (unless (equalp (parameter "oauth_signature_method") "HMAC-SHA1")
    (error "Signature method not passed or different from HMAC-SHA1"))
  (let ((signature (gethash (request) *signature-cache*)))
    (unless signature
      (error "This request is not signed"))
    ;; TODO
    t))


;;; nonce and timestamp checking
(defun check-nonce-and-timestamp ()
  ;; STUB
  (unless (parameter "oauth_timestamp")
    (error "Timestamp is missing"))
  (unless (parameter "oauth_nonce")
    (error "Nonce is missing"))
  t)


;;; version checking
(defun check-version ()
  (let ((version (parameter "oauth_version")))
    (unless (member version '("1.0" nil) :test #'equalp)
      (cerror "Not prepared to handle OAuth version other than 1.0" version))
    t))


;;; verification code checking
(defun check-verification-code ()
  (unless (equal (parameter "oauth_verifier")
                 (request-token-verification-code (get-supplied-request-token)))
    (error "Invalid request token verification code"))
  t)


;;; misc
(defun get-supplied-consumer-token ()
  (let ((consumer-key (parameter "oauth_consumer_key")))
    (unless consumer-key
      (error "Consumer key not supplied"))
    (let ((consumer-token (get-consumer-token consumer-key)))
      (unless consumer-token
        (error "Can't identify Consumer"))
      consumer-token)))


(defun get-supplied-callback-uri (&key allow-oob-callback-p)
  (let ((callback (parameter "oauth_callback")))
    (cond
      ((not callback)
       (error "No callback supplied"))
      ((and (not allow-oob-callback-p) (equal callback "oob"))
       (error "Not prepared for an OOB callback setup!"))
      (t
       callback))))

;;; request token management
(defvar *issued-request-tokens* (make-hash-table :test #'equalp))

(defun invalidate-request-token (request-token)
  (remhash (token-key request-token) *issued-request-tokens*))

(defun request-token-response (request-token &rest additional-parameters)
  (declare (ignore additional-parameters)) ; TODO not supported yet
  (url-encode (alist->query-string
                `(("oauth_token" . ,(token-key request-token))
                  ("oauth_token_secret" . ,(token-secret request-token))
                  ("oauth_callback_confirmed" . "true")))))

(defun validate-request-token-request (&key (request-token-ctor #'make-request-token)
                                            allow-oob-callback-p)
  "Check whether REQUEST is a valid request token request.
  
  Returns the supplied Consumer callback (a PURI:URI) or NIL if
  the callback is supposed to be transferred oob. [6.1.1]"
  ;; TODO: in case of error set appropriate 400/401 return code to
  ;; simplify debugging on the consumer side.
  ;;
  ;; TODO: raise specific errors instead of simple errors
  (assert (>= (length (normalized-parameters)) 6))
  (check-version)
  (check-nonce-and-timestamp)
  (check-signature)
  (let* ((consumer-token (get-supplied-consumer-token))
         (callback-uri (get-supplied-callback-uri :allow-oob-callback-p allow-oob-callback-p))
         (request-token (funcall request-token-ctor :consumer-token consumer-token)))
    (setf (request-token-callback-uri request-token) (when callback-uri
                                                       (puri:parse-uri callback-uri)))
    (setf (gethash (token-key request-token) *issued-request-tokens*) request-token)
    request-token))

(defun get-supplied-request-token (&key check-verification-code-p)
  "Utility function that extracts the Consumer-supplied request token
  from a list of normalized parameters. Guards against non-existing
  and unknown tokens. Returns the request token on success."
  ;; TODO: raise specific errors
  ;; TODO: check whether the supplied token matches the Consumer key
  (let ((request-token-key (parameter "oauth_token")))
    ;; check if the Consumer supplied a request token
    (unless request-token-key
      (error "No request token identifier (oauth_token) supplied."))
    ;; check if the supplied request token is known to us
    (let ((request-token (gethash request-token-key *issued-request-tokens*)))
      (unless request-token
        (error "Invalid request token."))
      (when check-verification-code-p
        (check-verification-code))
      ;; everything's looking good
      request-token)))


;;; access token management
(defvar *issued-access-tokens* (make-hash-table :test #'equalp))

(defun validate-access-token-request (&key (access-token-ctor #'make-access-token))
  (assert (= (length (normalized-parameters)) 8)) ; no user-supplied parameters allowed here, and the
                                     ; spec forbids duplicate oauth args per section 5.
  (check-version)
  (check-nonce-and-timestamp)
  (check-signature)
  (let* ((request-token (get-supplied-request-token :check-verification-code-p t))
         (access-token (funcall access-token-ctor :consumer (request-token-consumer request-token))))
    (setf (gethash (token-key access-token) *issued-access-tokens*) access-token)
    (prog1
        access-token
      (invalidate-request-token request-token))))

(defun access-token-response (access-token &rest additional-parameters)
  (declare (ignore additional-parameters)) ; TODO not supported yet
  (url-encode (alist->query-string
                `(("oauth_token" . ,(token-key access-token))
                  ("oauth_token_secret" . ,(token-secret access-token))))))


;;; protected resource access management [7]
(defun get-supplied-access-token ()
  "Utility function that extracts the Consumer-supplied request token
  from a list of normalized parameters. Guards against non-existing
  and unknown tokens. Returns the request token on success."
  ;; TODO: raise specific errors
  ;; TODO: check whether the supplied token matches the Consumer key
  (let ((access-token-key (parameter "oauth_token")))
    (unless access-token-key
      (error "No access token identifier (oauth_token) supplied."))
    ;; check if the supplied access token is known to us
    (let ((access-token (gethash access-token-key *issued-access-tokens*)))
      (unless access-token
        (error "Invalid access token."))
      access-token)))

(defun validate-access-token ()
  (assert (>= (length (normalized-parameters)) 6))
  (check-version)
  (check-nonce-and-timestamp)
  (check-signature)
  (let ((consumer-token (get-supplied-consumer-token))
        (access-token (get-supplied-access-token)))
    (unless (eq consumer-token (access-token-consumer access-token))
      (error "Access token ~S wasn't issued for Consumer ~S" access-token consumer-token))
    t))

