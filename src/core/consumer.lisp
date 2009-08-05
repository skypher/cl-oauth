
(in-package :oauth)

(defun obtain-request-token (uri request-method consumer-token signature-method
                             &key (version :1.0) user-parameters drakma-args)
  "Additional parameters will be stored in the USER-DATA slot of the
token."
  ;; TODO: support 1.0a too
  (let* ((parameters (append user-parameters
                             `(("oauth_consumer_key" . ,(token-key consumer-token))
                               ("oauth_signature_method" . ,signature-method)
                               ("oauth_timestamp" . ,(princ-to-string (get-universal-time)))
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
                (key (assoc "oauth_token" response :test #'equal))
                (secret (assoc "oauth_token_secret" response :test #'equal))
                (user-data (set-difference response '("oauth_token" "oauth_token_secret")
                                           :test (lambda (e1 e2)
                                                   (equal (car e1) e2)))))
           (assert key)
           (assert secret)
           (make-request-token :consumer consumer-token :key key :secret secret
                               :user-data user-data))
         (warn "Server returned status ~D" status))))) ; TODO: elaborate


(defun make-authorization-uri (uri &key (version :1.0) callback-uri request-token
                                         user-parameters drakma-args)
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


(defun authorize-request-token (request-token-lookup-fn)
  "Authorize a request token. Must be running in request context.

REQUEST-TOKEN-LOOKUP-FN will be called with the request token key
and must return a valid unauthorized request token or NIL.

Returns the authorized token or NIL if the token couldn't be found."
  ;; TODO test
  (let* ((token-key (assoc "oauth_token" (get-parameters) :test #'equal))
         (token (funcall request-token-lookup-fn token-key))
         (user-parameters (remove "oauth_token" (get-parameters) :test #'equal)))
    (cond
      (token
       (setf (request-token-authorized-p token) t)
       (setf (token-user-data token) user-parameters)
       token)
      (t
       (warn "Cannot find request token with key ~A~
              (never requested or already authorized)" token-key)
        nil))))


(defun obtain-access-token (uri consumer-token request-token signature-method
                             &key (request-method :post)
                                  (version :1.0)
                                  drakma-args)
  "Additional parameters will be stored in the USER-DATA slot of the
token. POST is recommended as request method. [6.3.1]" ; TODO 1.0a section number
  ;; TODO mark request token as used, but only on success
  (assert (request-token-authorized-p request-token))
  (let* ((parameters `(("oauth_consumer_key" . ,(token-key consumer-token))
                       ("oauth_token" . ,(token-key request-token))
                       ("oauth_signature_method" . ,signature-method)
                       ("oauth_timestamp" . ,(princ-to-string (get-universal-time)))
                       ("oauth_nonce" . ,(princ-to-string (random most-positive-fixnum)))
                       ("oauth_version" . ,(princ-to-string version))))
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
                (key (assoc "oauth_token" response :test #'equal))
                (secret (assoc "oauth_token_secret" response :test #'equal))
                (user-data (set-difference response '("oauth_token" "oauth_token_secret")
                                           :test (lambda (e1 e2)
                                                   (equal (car e1) e2)))))
           (assert key)
           (assert secret)
           (make-access-token :consumer consumer-token :key key :secret secret
                              :user-data user-data))
         (warn "Server returned status ~D" status)))))


(defun access-protected-resource (uri access-token request-method)
  nil)

;; test
(obtain-request-token "http://term.ie/oauth/example/request_token.php"
                      :GET (make-consumer-token) "HMAC-SHA1")

;(obtain-access-token
