
(in-package :oauth-test)

(def-suite service-provider :in oauth)

(in-suite service-provider)


(test check-version.valid
  (let ((*get-parameters* '(("oauth_version" . "1.0"))))
    (finishes (check-version))))

(test check-version.invalid
  (let ((*get-parameters* '(("oauth_version" . "foo"))))
    (signals error (check-version))))


(defmacro with-signed-request ((user-parameters &key signature-override) &body body)
  "Execute BODY in a signed request environment. SIGNATURE-OVERRIDE may be used
to provide a specific signature (which is supposed to be base64-urlencoded)."
  `(let ((consumer-token (make-consumer-token)))
     (register-token consumer-token)
     (let ((request-token (make-request-token)))
       (register-token request-token)
       (let* ((*request-object* (random most-positive-fixnum))
              (*request-method* :get)
              (*request-uri* "/foo")
              (parameters (append ,user-parameters
                                  `(("oauth_consumer_key" . ,(token-key consumer-token))
                                    ("oauth_token" . ,(token-key request-token)))))
              (signature (or ,signature-override
                             (encode-signature
                               (hmac-sha1 (signature-base-string :parameters (sort-parameters (copy-alist parameters)))
                                          (hmac-key (token-secret consumer-token) (token-secret request-token))))))
              (*get-parameters* (cons (cons "oauth_signature" signature) parameters)))
         (setf (gethash (request) oauth::*signature-cache*) signature)
         ,@body)
       (unregister-token request-token)
       (unregister-token consumer-token))))


;; TODO check for specific errors in the following tests.
(test check-signature.invalid-method
  (with-signed-request ('(("oauth_signature_method" . "foo")))
    (signals error (check-signature))))

(test check-signature.invalid
  (with-signed-request ('(("oauth_signature_method" . "hmac-sha1")) :signature-override "haha")
    (signals error (check-signature))))

(test check-signature.valid
  (with-signed-request ('(("oauth_signature_method" . "hmac-sha1")))
    (finishes (check-signature))))

(test check-signature.valid2
  (with-signed-request ('(("oauth_signature_method" . "HMAC-SHA1")))
    (finishes (check-signature))))

;(test (validate-request-token-request
;        :depends-on (and check-version check-signature))
;  (let ((*get-parameters* '(("oauth_version" . "1.0")
;                            ("

