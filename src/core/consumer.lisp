
(in-package :oauth)

(defun obtain-request-token (uri request-method consumer-token signature-method
		             &key user-parameters drakma-args)
  ;; TODO: adapt to 1.0a
  (let* ((parameters (append user-parameters
			     `(("oauth_consumer_key" . ,(token-key consumer-token))
			       ("oauth_signature_method" . ,signature-method)
			       ("oauth_timestamp" . ,(princ-to-string (get-universal-time)))
			       ("oauth_nonce" . ,(princ-to-string (random most-positive-fixnum)))
			       ("oauth_version" . "1.0"))))
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
		(secret (assoc "oauth_token_secret" response :test #'equal)))
	   (assert key)
	   (assert secret)
	   (make-request-token :consumer consumer-token :key key :secret secret))
	 (warn "Server returned status ~D" status))))) ; TODO: elaborate

;; test
(obtain-request-token "http://term.ie/oauth/example/request_token.php"
                      :GET (make-consumer-token) "HMAC-SHA1")

