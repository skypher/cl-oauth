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

(defun my-http-request (uri &key (request-method :post) parameters drakma-args)
  (let* ((param-string-encoded (alist->query-string parameters :include-leading-ampersand nil :url-encode t)))
    (apply #'drakma:http-request
	   (if (eql :post request-method)
	       uri
	       (uri-with-additional-query-part uri param-string-encoded))
	   :method request-method
	   :content (when (eql :post request-method)
		      param-string-encoded)
	   drakma-args)))

(defun obtain-request-token (uri consumer-token
                             &key (version :1.0) user-parameters drakma-args
                                  (timestamp (get-universal-time)) (request-method :post)
                                  callback-uri
                                  (signature-method :hmac-sha1))
  "Additional parameters will be stored in the USER-DATA slot of the
token."
  ;; TODO: support 1.0a too
  (let* ((parameters (append user-parameters
                             `(("oauth_consumer_key" . ,(token-key consumer-token))
                               ("oauth_signature_method" . ,(string signature-method))
                               ("oauth_callback" . ,(or callback-uri "oob"))
                               ("oauth_timestamp" . ,(princ-to-string timestamp))
                               ("oauth_nonce" . ,(princ-to-string (random most-positive-fixnum)))
                               ("oauth_version" . ,(princ-to-string version)))))
         (sbs (signature-base-string :uri uri :request-method request-method
                                     :parameters (sort-parameters (copy-alist parameters))))
         (key (hmac-key (token-secret consumer-token)))
         (signature (encode-signature (hmac-sha1 sbs key) nil))
         (signed-parameters (cons `("oauth_signature" . ,signature) parameters)))
    (multiple-value-bind (body status)
        (apply #'drakma:http-request uri :method request-method
                                         :parameters signed-parameters
                                         drakma-args)
      (if (eql status 200)
         (let* ((response (query-string->alist body))
                (key (cdr (assoc "oauth_token" response :test #'equal)))
                (secret (cdr (assoc "oauth_token_secret" response :test #'equal)))
                (user-data (set-difference response '("oauth_token" "oauth_token_secret")
                                           :test (lambda (e1 e2)
                                                   (equal (car e1) e2)))))
           (assert key)
           (assert secret)
           (make-request-token :consumer consumer-token :key key :secret secret
                               :callback-uri callback-uri :user-data user-data))
         (error "Server returned status ~D" status))))) ; TODO: elaborate


(defun make-authorization-uri (uri request-token &key (version :1.0) callback-uri user-parameters)
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
         (puri (puri:parse-uri uri)))
    (setf (puri:uri-query puri) (concatenate 'string (or (puri:uri-query puri) "")
                                                     (alist->query-string parameters)))
    puri))


(defun authorize-request-token-from-request (request-token-lookup-fn)
  "Authorize a request token. Must be running in request context.

REQUEST-TOKEN-LOOKUP-FN will be called with the request token key
and must return a valid unauthorized request token or NIL.

Returns the authorized token or NIL if the token couldn't be found."
  ;; TODO test
  (let* ((parameters (get-parameters))
         (token-key (cdr (assoc "oauth_token" parameters :test #'equal)))
         (verification-code (cdr (assoc "oauth_verifier" parameters :test #'equal))))
    (unless token-key
      (error "No token key passed"))
    (unless verification-code
      (error "No verification code passed"))
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


(defun obtain-access-token (uri consumer-token request-token
                             &key (request-method :post)
                                  (version :1.0)
                                  (timestamp (get-universal-time))
                                  drakma-args
			          (signature-method :hmac-sha1))
  "Additional parameters will be stored in the USER-DATA slot of the
token. POST is recommended as request method. [6.3.1]" ; TODO 1.0a section number
  ;; TODO mark request token as used, but only on success
  (assert (request-token-authorized-p request-token))
  (let* ((parameters `(("oauth_consumer_key" . ,(token-key consumer-token))
                       ("oauth_token" . ,(hunchentoot:url-decode (token-key request-token)))
                       ("oauth_verifier" . ,(request-token-verification-code request-token))
                       ("oauth_signature_method" . ,(string signature-method))
                       ("oauth_timestamp" . ,(princ-to-string timestamp))
                       ("oauth_nonce" . ,(princ-to-string (random most-positive-fixnum)))
                       ("oauth_version" . ,(princ-to-string version))))
         (sbs (signature-base-string :uri uri :request-method request-method
                                    :parameters (sort-parameters (copy-alist parameters))))
         (key (hmac-key (token-secret consumer-token) (token-secret request-token)))
         (signature (encode-signature (hmac-sha1 sbs key) nil))
         (signed-parameters (cons `("oauth_signature" . ,signature) parameters)))
    (multiple-value-bind (body status)
        (apply #'drakma:http-request uri :method request-method
                                         :parameters signed-parameters
                                         drakma-args)
      (if (eql status 200)
         (let* ((response (query-string->alist body))
                (key (cdr (assoc "oauth_token" response :test #'equal)))
                (secret (cdr (assoc "oauth_token_secret" response :test #'equal)))
                (user-data (remove-oauth-parameters response)))
           (assert key)
           (assert secret)
           (make-access-token :consumer consumer-token
                              :key (hunchentoot:url-decode key)
                              :secret (hunchentoot:url-decode secret)
                              :user-data user-data))
         (warn "Server returned status ~D" status)))))

(defun access-protected-resource (uri access-token consumer-token
				  &key
				  user-parameters
				  (version :1.0)
				  drakma-args
				  (request-method :post)
				  (signature-method :hmac-sha1))
  "Additional parameters will be stored in the USER-DATA slot of the
token."
  ;; TODO: support 1.0a too
  (let* ((parameters (append user-parameters
                             `(("oauth_consumer_key" . ,(token-key consumer-token))
			       ("oauth_token" . ,(token-key access-token))
                               ("oauth_signature_method" . ,(string signature-method))
                               ("oauth_timestamp" . ,(princ-to-string (get-universal-time)))
                               ("oauth_nonce" . ,(princ-to-string (random most-positive-fixnum)))
                               ("oauth_version" . ,(princ-to-string version)))))
         (sbs (signature-base-string :uri uri :request-method request-method
				     :parameters (sort-parameters (copy-alist parameters))))
         (key (hmac-key (token-secret consumer-token) (token-secret access-token)))
         (signature (encode-signature (hmac-sha1 sbs key) nil))
         (signed-parameters (cons `("oauth_signature" . ,signature) parameters)))
    (multiple-value-bind (body status)
        (my-http-request uri
			 :request-method request-method
			 :parameters signed-parameters
			 :drakma-args drakma-args)
      (if (eql status 200)
	  (values body status)
	  (progn (warn "Server returned status ~D" status)
		 (values body status))))))


;; test
;(obtain-request-token "http://term.ie/oauth/example/request_token.php"
;                      :GET (make-consumer-token) "HMAC-SHA1")

;(obtain-access-token
