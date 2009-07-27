
(defmacro without-package-variance-warnings (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (handler-bind (#+sbcl(sb-int:package-at-variance #'muffle-warning))
       ,@body)))

(without-package-variance-warnings
  (defpackage #:cl-oauth
    (:nicknames #:oauth)
    (:use #:cl #:anaphora #:f-underscore)
    (:import-from #:hunchentoot
                  #:create-prefix-dispatcher
                  #:*dispatch-table*)
    (:import-from #:alexandria #:with-unique-names #:curry #:rcurry)
    (:import-from #:split-sequence #:split-sequence)
    (:export
      ;;; tokens
      #:token
      #:token-key
      #:token-secret

      #:register-token
      #:unregister-token

      #:consumer-token
      #:make-consumer-token

      #:request-token
      #:make-request-token
      #:request-token-authorized-p
      #:request-token-callback-uri
      #:request-token-consumer
      #:request-token-verification-code

      #:access-token
      #:make-access-token

      ;;; crypto
      #:signature-base-string
      #:hmac-key
      #:hmac-sha1
      #:encode-signature
      
      ;;; service provider
      #:check-version
      #:check-nonce-and-timestamp
      #:check-signature
      #:check-verification-code

      #:validate-request-token-request
      #:request-token-reponse

      #:get-supplied-request-token
      #:finalize-callback-uri

      #:validate-access-token-request
      )))

