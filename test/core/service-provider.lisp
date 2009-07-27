
(in-package :oauth-test)

(def-suite service-provider :in oauth)

(in-suite service-provider)


(test check-version.valid
  (let ((*get-parameters* '(("oauth_version" . "1.0"))))
    (finishes (check-version))))

(test check-version.invalid
  (let ((*get-parameters* '(("oauth_version" . "foo"))))
    (signals error (check-version))))


(defmacro with-signed-request ((&key user-parameters signature-override
                                     (signature-method "HMAC-SHA1")
                                     (consumer-token (make-consumer-token))
                                     token)
                               &body body)
  "Execute BODY in a signed request environment. SIGNATURE-OVERRIDE may be used
to provide a specific signature (which is supposed to be base64-urlencoded)."
  `(progn
     (register-token ,consumer-token)
     (when ,token
       (register-token ,token))
     (let* ((*request-object* (random most-positive-fixnum))
            (*request-method* :get)
            (*request-uri* "/foo")
            (parameters (append ,user-parameters
                                (list (cons "oauth_signature_method" ,signature-method)
                                      (cons "oauth_consumer_key" (token-key ,consumer-token)))
                                (when ,token
                                  (list (cons "oauth_token" (token-key ,token))))))
            (signature (or ,signature-override
                           (encode-signature
                             (hmac-sha1 (signature-base-string :parameters (sort-parameters
                                                                             (copy-alist parameters)))
                                        (hmac-key (token-secret ,consumer-token)
                                                  (when ,token (token-secret ,token)))))))
            (*get-parameters* (cons (cons "oauth_signature" signature) parameters)))
       (setf (gethash (request) oauth::*signature-cache*) signature)
       ,@body)
     (when ,token
       (unregister-token ,token))
     (unregister-token ,consumer-token)))


;; TODO check for specific errors in the following tests.
(test check-signature.invalid-method
  (with-signed-request (:signature-method "foo")
    (signals error (check-signature))))

(test check-signature.invalid
  (with-signed-request (:signature-override "haha")
    (signals error (check-signature))))

(test check-signature.valid
  (with-signed-request ()
    (finishes (check-signature))))

(test check-signature.valid2
  (with-signed-request ()
    (finishes (check-signature))))


;; high-level SP API
;(test (validate-request-token-request
;        :depends-on (and check-version check-signature.valid))
;  (with-signed-request ()
;    (validate-request-token-request)))


