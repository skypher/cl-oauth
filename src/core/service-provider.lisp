
(in-package :oauth)

(defvar *protocol-version* :1.0)

;;;; Service provider infrastructure

;;;; TODO: need to store application-specific data somewhere.


(defun finalize-callback-uri (request-token)
  "Prepares the callback URI of REQUEST-TOKEN for
  redirection."
  (let ((uri (request-token-callback-uri request-token)))
    (setf (puri:uri-query uri)
          (concatenate 'string (or (puri:uri-query uri) "")
                       (if (puri:uri-query uri) "&" "")
                       "oauth_token="
                         (url-encode (token-key request-token))
                       "&oauth_verifier="
                         (url-encode (request-token-verification-code request-token))))
    uri))


;;; Consumer management
(defvar *registered-consumers* (make-hash-table :test #'equalp))

(defmethod register-token ((token consumer-token))
  (setf (gethash (token-key token) *registered-consumers*) token)
  token)

(defmethod unregister-token ((token consumer-token))
  (remhash (token-key token) *registered-consumers*))

(defun get-consumer-token (key)
  (gethash key *registered-consumers*))

(defmacro ignore-oauth-errors (&body body)
  `(handler-case (progn ,@body)
     (http-error (condition) (values nil condition))))

;;; signature checking
(defun check-signature ()
  (unless (equalp (parameter "oauth_signature_method") "HMAC-SHA1")
    (raise-error 'bad-request "Signature method not passed or different from HMAC-SHA1"))
  (let* ((supplied-signature (gethash (request) *signature-cache*))
         ;; TODO: do not bluntly ignore all errors. Factor out into GET-TOKEN
         (consumer-secret (ignore-errors
                            (token-secret
                              (get-consumer-token (parameter "oauth_consumer_key")))))
         (token-secret (ignore-errors
                         (token-secret (or (ignore-oauth-errors (get-supplied-request-token))
                                           (ignore-oauth-errors (get-supplied-access-token)))))))
    (unless supplied-signature
      (raise-error 'bad-request "This request is not signed"))
    (unless consumer-secret
      (raise-error 'unauthorized "Invalid consumer"))
    ;; now calculate the signature and check for match
    (let* ((signature-base-string (signature-base-string))
           (hmac-key (hmac-key consumer-secret token-secret))
           (signature (hmac-sha1 signature-base-string hmac-key))
           (encoded-signature (encode-signature signature nil)))
      (unless (equal encoded-signature supplied-signature)
        (format t "calculated: ~S / supplied: ~S~%" encoded-signature supplied-signature)
        (raise-error 'unauthorized "Invalid signature")))
    t))


;;; nonce and timestamp checking
(defun check-nonce-and-timestamp (consumer-token)
  ;; TODO: nonce checking
  (unless (parameter "oauth_timestamp")
      (raise-error 'bad-request "Missing Timestamp"))
  (let ((timestamp (ignore-errors (parse-integer (parameter "oauth_timestamp"))))
        (nonce (parameter "oauth_nonce")))
    (unless timestamp
      (raise-error 'unauthorized "Malformed Timestamp"))
    (unless nonce
      (raise-error 'bad-request "Missing nonce"))
    (unless (>= timestamp (consumer-token-last-timestamp consumer-token))
      (raise-error 'unauthorized "Invalid timestamp"))
    t))


;;; version checking
(defun check-version ()
  (let ((version (parameter "oauth_version")))
    (unless (member version '("1.0" nil) :test #'equalp)
      (raise-error 'bad-request "Not prepared to handle OAuth version other than 1.0" version))
    t))


;;; verification code checking
(defun check-verification-code ()
  (unless (equal (parameter "oauth_verifier")
                 (request-token-verification-code (get-supplied-request-token)))
    (raise-error 'unauthorized "Invalid verification code"))
  t)


;;; misc
(defun get-supplied-consumer-token ()
  (let ((consumer-key (parameter "oauth_consumer_key")))
    (unless consumer-key
      (raise-error 'bad-request "Consumer key not supplied"))
    (let ((consumer-token (get-consumer-token consumer-key)))
      (unless consumer-token
        (raise-error 'unauthorized "Can't identify Consumer"))
      consumer-token)))


(defun get-supplied-callback-uri (&key allow-oob-callback-p
                                       (allow-none (eq *protocol-version* :1.0)))
  (let ((callback (parameter "oauth_callback")))
    (cond
      ((and (not allow-none) (not callback))
       (raise-error 'bad-request "No callback supplied"))
      ((and (not allow-oob-callback-p) (equal callback "oob"))
       (raise-error 'bad-request "Not prepared for an OOB callback setup!"))
      (t
       callback))))


;;; request token management
(defvar *issued-request-tokens* (make-hash-table :test #'equalp))

(defmethod register-token ((token request-token))
  ;; TODO: already registered?
  (setf (gethash (token-key token) *issued-request-tokens*) token))

(defmethod unregister-token ((token request-token))
  (remhash (token-key token) *issued-request-tokens*))

(defun invalidate-request-token (request-token)
  (remhash (token-key request-token) *issued-request-tokens*))

(defun make-response (alist)
  "[5.3]"
  (alist->query-string
    (mapcar (lambda (cons)
              (cons (url-encode (car cons))
                    (url-encode (cdr cons))))
            alist)
    :include-leading-ampersand nil))

(defun request-token-response (request-token &rest additional-parameters)
  "Respond to a valid request token request. [6.1.2]"
  (assert (notany #'oauth-parameter-p additional-parameters))
  (make-response
    (append
      `(("oauth_token" . ,(token-key request-token))
        ("oauth_token_secret" . ,(token-secret request-token))
        ("oauth_callback_confirmed" . "true"))
      additional-parameters)))

(defun validate-request-token-request (&key (request-token-ctor #'make-request-token)
                                            allow-oob-callback-p)
  "Check whether REQUEST is a valid request token request.
  
  Returns the supplied Consumer callback (a PURI:URI) or NIL if
  the callback is supposed to be transferred oob. [6.1.1]"
  (protocol-assert (>= (length (normalized-parameters))
                       (case *protocol-version*
                         ;; callbacks were introduced in 1.0a
                         (1.0 4)
                         (t 6 5))))
  (check-version)
  (check-signature)
  (let ((consumer-token (get-supplied-consumer-token)))
    (check-nonce-and-timestamp consumer-token)
    (let* ((callback-uri (get-supplied-callback-uri :allow-oob-callback-p allow-oob-callback-p
                                                    :allow-none t))
           (request-token (funcall request-token-ctor :consumer consumer-token
                                   :callback-uri (when callback-uri
                                                   (puri:parse-uri callback-uri))
                                   :user-data (remove-oauth-parameters (normalized-parameters)))))
      (register-token request-token)
      request-token)))

(defun get-supplied-request-token (&key check-verification-code-p)
  "Utility function that extracts the Consumer-supplied request token
  from a list of normalized parameters. Guards against non-existing
  and unknown tokens. Returns the request token on success."
  ;; TODO: check whether the supplied token matches the Consumer key
  (let ((request-token-key (parameter "oauth_token")))
    ;; check if the Consumer supplied a request token
    (unless request-token-key
      (raise-error 'bad-request "Missing request token"))
    ;; check if the supplied request token is known to us
    (let ((request-token (gethash request-token-key *issued-request-tokens*)))
      (unless request-token
        (raise-error 'unauthorized "Invalid request token"))
      (when check-verification-code-p
        (check-verification-code))
      ;; everything's looking good
      request-token)))


;;; access token management
(defvar *issued-access-tokens* (make-hash-table :test #'equalp))

(defmethod register-token ((token access-token))
  (setf (gethash (token-key token) *issued-access-tokens*) token))

(defmethod unregister-token ((token access-token))
  (remhash (token-key token) *issued-access-tokens*))

(defun validate-access-token-request (&key (access-token-ctor #'make-access-token))
  ;; no user-supplied parameters allowed here, and the
  ;; spec forbids duplicate oauth args per section 5.
  ;; moreover we don't count the oauth_signature parameter as it isn't
  ;; part of the normalized parameter list.
  (protocol-assert (multiple-value-call #'between (length (normalized-parameters))
             (case *protocol-version*
               (1.0 (values 5 6))
               (t 6 (values 6 7)))))
  (format t "foo~%")
  (protocol-assert (null (remove-oauth-parameters (normalized-parameters))))
  (format t "bar~%")
  (check-version)
  (check-signature)
  (let* ((request-token (get-supplied-request-token
                          :check-verification-code-p (not (eq *protocol-version* :1.0))))
         (consumer (token-consumer request-token)))
    (check-nonce-and-timestamp consumer)
    (let ((access-token (funcall access-token-ctor :consumer consumer)))
      (register-token access-token)
      (prog1
          access-token
        (invalidate-request-token request-token)))))

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
  ;; TODO: check whether the supplied token matches the Consumer key
  (let ((access-token-key (parameter "oauth_token")))
    (unless access-token-key
      (raise-error 'bad-request "Missing access token"))
    ;; check if the supplied access token is known to us
    (let ((access-token (gethash access-token-key *issued-access-tokens*)))
      (unless access-token
        (raise-error 'unauthorized "Invalid access token"))
      access-token)))

(defun validate-access-token ()
  (protocol-assert (>= (length (normalized-parameters)) 6))
  (check-version)
  (check-signature)
  (let ((consumer-token (get-supplied-consumer-token)))
    (check-nonce-and-timestamp consumer-token)
    (let ((access-token (get-supplied-access-token)))
      (unless (eq consumer-token (token-consumer access-token))
        (raise-error 'unauthorized "Access token ~S wasn't issued for Consumer ~S" access-token consumer-token))
      t)))

