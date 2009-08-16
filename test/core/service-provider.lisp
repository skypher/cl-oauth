
(in-package :oauth-test)

(def-suite service-provider :in oauth)

(in-suite service-provider)

;; TODO tests for check-nonce-and-timestamp


(test check-version.valid
  (let ((*get-parameters* '(("oauth_version" . "1.0"))))
    (finishes (check-version))))

(test check-version.invalid
  (let ((*get-parameters* '(("oauth_version" . "foo"))))
    (signals error (check-version))))


(defmacro with-signed-request ((&key user-parameters 

                                     (version "1.0")
                                     (timestamp (get-universal-time))
                                     (nonce (random most-positive-fixnum))

                                     signature-override
                                     (signature-method "HMAC-SHA1")

                                     (consumer-token (make-consumer-token))
                                     
                                     token
                                     verification-code)
                               &body body)
  "Execute BODY in a signed request environment. SIGNATURE-OVERRIDE may be used
to provide a specific signature (which is supposed to be base64-urlencoded)."
  `(progn
     (register-token ,consumer-token)
     (when ,token
       (assert (typep ,token '(or request-token access-token)))
       (setf (token-consumer ,token) ,consumer-token)
       (register-token ,token))
     (let* ((*request-object* (random most-positive-fixnum))
            (*request-method* :get)
            (*request-uri* "/foo")
            (parameters (append ',user-parameters
                                (list (cons "oauth_version" ,version)
                                      (cons "oauth_signature_method" ,signature-method)
                                      (cons "oauth_consumer_key" (token-key ,consumer-token))
                                      (cons "oauth_timestamp" (princ-to-string ,timestamp))
                                      (cons "oauth_nonce" (princ-to-string ,nonce)))
                                (when ,token
                                  (list (cons "oauth_token" (token-key ,token))))
                                (when (and ,token (typep ,token 'request-token))
                                  (list (cons "oauth_verifier" (or ,verification-code
                                                                   (request-token-verification-code ,token)))))))
            (signature (or ,signature-override
                           (encode-signature
                             (hmac-sha1 (signature-base-string :parameters (sort-parameters
                                                                             (copy-alist parameters)))
                                        (hmac-key (token-secret ,consumer-token)
                                                  (when ,token (token-secret ,token))))
                             nil)))
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


;;;; high-level API

;;; phase 1
(test (validate-request-token-request.oob
        :depends-on (and check-version.valid check-signature.valid))
  (with-signed-request (:user-parameters (("oauth_callback" . "oob")))
    (is (typep (validate-request-token-request :allow-oob-callback-p t) 'request-token))))

(test (validate-request-token-request.oob-disallowed
        :depends-on (and check-version.valid check-signature.valid))
  (with-signed-request (:user-parameters (("oauth_callback" . "oob")))
    (signals error (validate-request-token-request :allow-oob-callback-p nil))))

(test (validate-request-token-request.callback-uri
        :depends-on (and check-version.valid check-signature.valid))
  (with-signed-request (:user-parameters (("oauth_callback" . "http://example.com/bar")))
    (is (typep (validate-request-token-request :allow-oob-callback-p nil) 'request-token))))


;;; phase 2
(test (validate-access-token-request.valid-request-token
        :depends-on (and check-version.valid check-signature.valid))
  (let ((request-token (make-request-token))
        (*protocol-version* :1.0a))
    (setf (request-token-authorized-p request-token) t)
    (with-signed-request (:token request-token)
      (is (typep (validate-access-token-request) 'access-token)))))

;; TODO more tests, esp. for invalid requests.


;;; phase 3
(test (validate-access-token.valid
        :depends-on (and check-version.valid check-signature.valid))
  (let ((access-token (make-access-token)))
    (with-signed-request (:token access-token)
      (is (eq t (validate-access-token))))))

