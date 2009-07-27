
(in-package :oauth-test)

(def-suite tokens :in oauth)

(in-suite tokens)

(test request-token.not-authorized-by-default
  (is (not (request-token-authorized-p (make-request-token)))))

(test token-printer.deals-with-unbound-slots
  (finishes (write-to-string (make-request-token))))

