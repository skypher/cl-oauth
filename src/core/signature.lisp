
(in-package :oauth)

(defun signature-base-string (&key (uri (request-uri))
                                   (request-method (request-method))
                                   (parameters (normalized-parameters)))
  (concatenate 'string (string-upcase (princ-to-string request-method))
                       "&" (url-encode
                             (normalize-uri uri))
                       "&" (url-encode
                             (alist->query-string parameters
                                                  :url-encode t
                                                  :include-leading-ampersand nil))))

(declaim (notinline hmac-key)) ; we want to trace this when debugging. 
(defun hmac-key (consumer-secret &optional token-secret)
  "9.2"
  (concatenate 'string (url-encode consumer-secret) "&" (url-encode (or token-secret ""))))

(defun encode-signature (octets url-encode-p)
  "9.2.1"
  (let ((base64 (cl-base64:usb8-array-to-base64-string octets)))
    (if url-encode-p
      (url-encode base64)
      base64)))

