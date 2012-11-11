
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
    (:import-from #:alexandria #:with-unique-names #:curry #:rcurry #:ensure-list #:compose)
    (:import-from #:split-sequence #:split-sequence)
    (:export
      #:*protocol-version*

      ;;; error handling
      #:http-error
      #:bad-request
      #:unauthorized
      #:raise-error
      #:default-error-handler
      #:protocol-assert

      ;;; tokens
      #:token
      #:token-key
      #:token-secret
      #:token-user-data

      #:token-consumer

      #:register-token
      #:unregister-token

      #:consumer-token
      #:make-consumer-token

      #:request-token
      #:make-request-token
      #:request-token-authorized-p
      #:request-token-callback-uri
      #:request-token-verification-code

      #:access-token
      #:make-access-token
      #:access-token-session-handle
      #:access-token-expires
      #:access-token-authorization-expires
      #:access-token-expired-p

      ;;; consumer functions
      #:obtain-access-token
      #:authorize-request-token
      #:authorize-request-token-from-request
      #:make-authorization-uri
      #:obtain-request-token
      #:access-protected-resource

      ;;; crypto
      #:signature-base-string
      #:hmac-key
      #:hmac-sha1
      #:encode-url
      #:encode-signature
      
      ;;; parameters
      #:remove-auth-parameters
      #:normalized-parameters

      ;;; service provider
      #:check-version
      #:check-nonce-and-timestamp
      #:check-signature
      #:check-verification-code

      #:validate-request-token-request
      #:request-token-response

      #:get-supplied-request-token
      #:finalize-callback-uri

      #:validate-access-token-request

      #:validate-access-token

      #:make-response
      )))

