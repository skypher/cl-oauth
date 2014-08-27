(in-package :oauth)

(defun uri-with-additional-query-part (uri query-part)
  "Given a URI string or PURI uri, adds the string QUERY-PART to the end of the URI.  If
it has query params already they are added onto it."
  (let* ((puri (puri:uri uri))
	 (existing-query-part (puri:uri-query puri)))
    (setf (puri:uri-query puri)
	  (if (and existing-query-part query-part)
	      (concatenate 'string existing-query-part "&" query-part)
	      (or existing-query-part query-part)))
    (puri:render-uri puri nil)))

(defun build-auth-string (parameters)
  (format nil "OAuth ~{~A=~S~^, ~}"
          (alexandria:flatten (mapcar
                                (lambda (x y) (list x y))
                                (mapcar (compose #'url-encode #'car) parameters)
                                (mapcar (compose #'url-encode #'cdr) parameters)))))

(defun http-request
    (uri &key (auth-location :header) (method :get) auth-parameters parameters additional-headers drakma-args)
  (apply #'drakma:http-request
         uri
         :method method
         :parameters (if (eq auth-location :parameters)
                         (append parameters auth-parameters)
                         parameters)
         :additional-headers (if (eq auth-location :header)
                                 (cons `("Authorization" . ,(build-auth-string auth-parameters))
                                       additional-headers)
                                 additional-headers)
         drakma-args))

;;; SBCL 1.1.6 on OS X does not generate proper random values with (random most-positive-fixnum).
(defun generate-nonce (&optional (size 30))
  (with-open-file (in "/dev/urandom" :direction :input :element-type '(unsigned-byte 8))
    (with-output-to-string (out)
      (loop :repeat size
         :do (write (read-byte in) :stream out :pretty nil :base 36)))))

(defun generate-auth-parameters
    (consumer signature-method timestamp version &optional token)
  (let ((parameters `(("oauth_consumer_key" . ,(token-key consumer))
                      ("oauth_signature_method" . ,(string signature-method))
                      ("oauth_timestamp" . ,(princ-to-string timestamp))
                      #+unix ("oauth_nonce" . ,(generate-nonce))
                      #-unix ("oauth_nonce" . ,(princ-to-string
                                                (random most-positive-fixnum)))
                      ("oauth_version" . ,(princ-to-string version)))))
    (if token
        (cons `("oauth_token" . ,(url-decode (token-key token))) parameters)
        parameters)))

(defun obtain-request-token (uri consumer-token
                             &key (version :1.0) user-parameters drakma-args
                                  (timestamp (get-unix-time))
                                  (auth-location :header)
                                  (request-method :post)
                                  callback-uri
                                  additional-headers
                                  (signature-method :hmac-sha1)
                                  (include-user-parameters-in-signature-p t))
  "Additional parameters will be stored in the USER-DATA slot of the token."
  ;; TODO: support 1.0a too
  (let* ((callback-uri (or callback-uri "oob"))
         (auth-parameters (cons `("oauth_callback" . ,callback-uri)
                                (generate-auth-parameters consumer-token
                                                          signature-method
                                                          timestamp
                                                          version)))
         (sbs (signature-base-string :uri uri :request-method request-method
                                     :parameters (sort-parameters (copy-alist (if include-user-parameters-in-signature-p
                                                                                  (append user-parameters auth-parameters)
                                                                                  auth-parameters)))))
         (key (hmac-key (token-secret consumer-token)))
         (signature (encode-signature (hmac-sha1 sbs key) nil))
         (signed-parameters (cons `("oauth_signature" . ,signature) auth-parameters)))
    (multiple-value-bind (body status)
        (http-request uri
                      :method request-method
                      :auth-location auth-location
                      :auth-parameters signed-parameters
                      :parameters user-parameters
                      :additional-headers additional-headers
                      :drakma-args drakma-args)
      (if (eql status 200)
          (let* ((response (query-string->alist (typecase body
                                                  (string body)
                                                  (t (map 'string #'code-char body)))))
                 (key (cdr (assoc "oauth_token" response :test #'equal)))
                 (secret (cdr (assoc "oauth_token_secret" response :test #'equal)))
                 (user-data (set-difference response '("oauth_token" "oauth_token_secret")
                                            :test (lambda (e1 e2)
                                                    (equal (car e1) e2)))))
            (assert key)
            (assert secret)
            (make-request-token :consumer consumer-token :key key :secret secret ;; TODO url-decode
                                :callback-uri (puri:uri callback-uri) :user-data user-data))
          (error "Server returned status ~D: ~A" status body))))) 


(defun make-authorization-uri (uri request-token &key callback-uri user-parameters)
  "Return the service provider's authorization URI. Use the resulting PURI
for a redirect. [6.2.1] in 1.0." ; TODO 1.0a section number
  ;; TODO: does 1.0 support oob callbacks?
  (when (and request-token (request-token-authorized-p request-token))
    (error "Request token ~A already authorized" request-token))
  (let* ((parameters (append user-parameters
                             (when request-token
                               (list (cons "oauth_token" (token-key request-token))))
                             (when callback-uri
                               (list (cons "oauth_callback" callback-uri)))))
         (puri (puri:copy-uri (puri:parse-uri uri))))
    (setf (puri:uri-query puri)
          (if (puri:uri-query puri)
              (concatenate 'string
                           (puri:uri-query puri)
                           (alist->query-string parameters))
              (alist->query-string parameters :include-leading-ampersand nil)))
    puri))


(defun authorize-request-token-from-request (request-token-lookup-fn)
  "Authorize a request token. Must be running in request context.

REQUEST-TOKEN-LOOKUP-FN will be called with the request token key
and must return a valid unauthorized request token or NIL.

Returns the authorized token or NIL if the token couldn't be found."
  (let* ((parameters (get-parameters))
         (token-key (cdr (assoc "oauth_token" parameters :test #'equal)))
         (verification-code (cdr (assoc "oauth_verifier" parameters :test #'equal))))
    (unless token-key
      (error "No token key passed"))
    (let ((token (funcall request-token-lookup-fn token-key))
          (user-parameters (remove-oauth-parameters parameters)))
      (cond
        (token
         (authorize-request-token token)
         (setf (request-token-verification-code token) verification-code)
         (setf (token-user-data token) user-parameters)
         token)
        (t
         (error "Cannot find request token with key ~A ~
                (never requested or already authorized)" token-key))))))


(defun authorize-request-token (request-token)
  "Authorize a request token explicitly.  Returns the authorized token."
  ;; TODO test
  (setf (request-token-authorized-p request-token) t)
  request-token)

(defun obtain-access-token (uri request-or-access-token &key
                            (consumer-token (token-consumer request-or-access-token))
                            (request-method :post)
                            (auth-location :header)
                            (version :1.0)
                            (timestamp (get-unix-time))
                            xauth-username xauth-password
                            drakma-args
			    (signature-method :hmac-sha1))
  "Additional parameters will be stored in the USER-DATA slot of the
token. POST is recommended as request method. [6.3.1]" ; TODO 1.0a section number
  (let ((refresh-p (typep request-or-access-token 'access-token)))
    (when (and request-or-access-token
               (not refresh-p))
      (assert (request-token-authorized-p request-or-access-token)))
    (let* ((parameters (append
                        (generate-auth-parameters consumer-token
                                                  signature-method
                                                  timestamp
                                                  version
                                                  request-or-access-token)
                        (cond
                          (refresh-p
                           `(("oauth_session_handle" . ,(access-token-session-handle
                                                         request-or-access-token))))
                          ((null request-or-access-token)
                           `(("x_auth_mode" . "client_auth")
                             ("x_auth_username" . ,xauth-username)
                             ("x_auth_password" . ,xauth-password)))
                          (t
                           (awhen (request-token-verification-code request-or-access-token)
                             `(("oauth_verifier" . ,it)))))))
           (sbs (signature-base-string :uri uri :request-method request-method
                                       :parameters (sort-parameters (copy-alist parameters))))
           (key (hmac-key (token-secret consumer-token)
                          (when request-or-access-token
                            (url-decode (token-secret request-or-access-token)))))
           (signature (encode-signature (hmac-sha1 sbs key) nil))
           (signed-parameters (cons `("oauth_signature" . ,signature) parameters)))
      (multiple-value-bind (body status)
          (http-request uri
                        :method request-method
                        :auth-location auth-location
                        :auth-parameters signed-parameters
                        :drakma-args drakma-args)
        (if (eql status 200)
            (let ((response (query-string->alist (if (stringp body)
                                                     body
                                                     (babel:octets-to-string body)))))
              (flet ((field (name)
                       (cdr (assoc name response :test #'equal))))
                (let ((key (field "oauth_token"))
                      (secret (field "oauth_token_secret"))
                      (session-handle (field "oauth_session_handle"))
                      (expires (awhen (field "oauth_expires_in")
                                 (parse-integer it)))
                      (authorization-expires (awhen (field "oauth_authorization_expires_in")
                                               (parse-integer it)))
                      (user-data (remove-oauth-parameters response)))
                  (assert key)
                  (assert secret)
                  (make-access-token :consumer consumer-token
                                     :key (url-decode key)
                                     :secret (url-decode secret)
                                     :session-handle session-handle
                                     :expires (awhen expires
                                                (+ (get-universal-time) it))
                                     :authorization-expires (awhen authorization-expires
                                                              (+ (get-universal-time) it))
                                     :origin-uri uri
                                     :user-data user-data))))
            (error "Couldn't obtain access token: server returned status ~D" status))))))

(defun refresh-access-token (access-token)
  (obtain-access-token (access-token-origin-uri access-token) access-token))

(defun maybe-refresh-access-token (access-token &optional on-refresh)
  (if (access-token-expired-p access-token)
    (let ((new-token (refresh-access-token access-token)))
      (when on-refresh
        (funcall on-refresh new-token))
      new-token)
    access-token))

(defun get-problem-report-from-headers (headers)
  (let ((authenticate-header (drakma:header-value :www-authenticate headers)))
    (when (and authenticate-header (>= (length authenticate-header) 5))
      (let ((type (subseq authenticate-header 0 5)))
        (when (and (equalp type "OAuth") 
		   (> (length authenticate-header) 5))
          (let ((parameters (mapcar (lambda (token)
                                      (destructuring-bind (name value)
                                          (split-sequence #\= token)
                                        (cons name (string-trim '(#\") value))))
                                    (drakma:split-tokens
                                      (subseq authenticate-header 6)))))
            parameters))))))

(defun get-problem-report (headers body)
  (declare (ignore body)) ; TODO
  (let ((from-headers (get-problem-report-from-headers headers)))
    from-headers))

(defun access-protected-resource (uri access-token
                                  &rest kwargs
                                  &key
                                    (consumer-token (token-consumer access-token))
                                    on-refresh
                                    (timestamp (get-unix-time))
                                    user-parameters
                                    additional-headers
                                    (version :1.0)
                                    drakma-args
                                    (auth-location :header)
                                    (request-method :get)
                                    (signature-method :hmac-sha1)
                                    (include-user-parameters-in-signature-p t))
  "Access the protected resource at URI using ACCESS-TOKEN.

If the token contains OAuth Session information it will be checked for
validity before the request is made. Should the server notify us that
it has prematurely expired the token will be refresh as well and the
request sent again using the new token. ON-REFRESH will be called
whenever the access token is renewed."
  (setf access-token (maybe-refresh-access-token access-token on-refresh))
  (multiple-value-bind (normalized-uri query-string-parameters) (normalize-uri uri)
    (let* ((auth-parameters (generate-auth-parameters consumer-token
                                                      signature-method
                                                      timestamp
                                                      version
                                                      access-token))
           (sbs (signature-base-string :uri normalized-uri
                                       :request-method request-method
                                       :parameters (sort-parameters (copy-alist (if include-user-parameters-in-signature-p
                                                                                    (append query-string-parameters user-parameters auth-parameters)
                                                                                    auth-parameters)))))
           (key (hmac-key (token-secret consumer-token) (token-secret access-token)))
           (signature (encode-signature (hmac-sha1 sbs key) nil))
           (signed-parameters (cons `("oauth_signature" . ,signature) auth-parameters)))
      (when (and (eql request-method :post)
                 user-parameters)
        (assert (and (not (getf drakma-args :content-type))
                     (not (getf drakma-args :content)))
                () "User parameters and content/content-type in drakma arguments cannot be combined")
        (setf drakma-args (list* :content-type "application/x-www-form-urlencoded"
                                 :content (alist->query-string user-parameters
                                                               :url-encode t
                                                               :include-leading-ampersand nil)
                                 drakma-args)))
      (multiple-value-bind (body status headers)
          (http-request uri
                        :method request-method
                        :auth-location auth-location
                        :auth-parameters signed-parameters
                        :parameters user-parameters
                        :additional-headers additional-headers
                        :drakma-args drakma-args)
        (if (eql status 200)
          (values body status)
          (let* ((problem-report (get-problem-report headers body))
                 (problem-hint (cdr (assoc "oauth_problem" problem-report :test #'equalp)))
                 (problem-advice (cdr (assoc "oauth_problem_advice" problem-report :test #'equalp))))
            (cond
              ((and (eql status 401)
                    (equalp problem-hint "token_expired"))
               (format t "INFO: refreshing access token~%")
               (let ((new-token (refresh-access-token access-token)))
                 (when on-refresh
                   (funcall on-refresh new-token))
                 (apply #'access-protected-resource uri new-token kwargs)))
              (t
               (values body status problem-hint problem-advice)))))))))

