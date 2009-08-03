
(in-package :oauth)

(defun obtain-request-token (uri request-method consumer-token signature-method
		             &key user-parameters drakma-args)
  (let* ((parameters (append user-parameters
			     `(("oauth_consumer_key" . ,(token-key consumer-token))
			       ("oauth_signature_method" . ,signature-method)
			       ("oauth_timestamp" . ,(get-universal-time))
			       ("oauth_nonce" . ,(random most-positive-fixnum))
			       ("oauth_version" . "1.0"))))
	 (sbs (signature-base-string :uri uri :request-method request-method
				    :parameters (sort-parameters (copy-alist parameters))))
	 (key (hmac-key (token-secret consumer-token)))
	 (signature (encode-signature (hmac-sha1 sbs key) nil))
	 (signed-parameters (cons `("oauth_signature" . ,signature) parameters))
	 (response (apply #'drakma:http-request uri :method request-method
			                            :parameters signed-parameters
						drakma-args)))
    response))

