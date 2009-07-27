
(in-package :oauth)

(defun string-or-octets->octets (x)
  (etypecase x
    (string (babel:string-to-octets x))
    ((simple-array (unsigned-byte 8)) x)))

(defun hmac-sha1 (s key)
  (let* ((s (string-or-octets->octets s))
         (key (string-or-octets->octets key))
         (hmac (ironclad:make-hmac key 'ironclad:sha1)))
    (ironclad:update-hmac hmac s)
    (ironclad:hmac-digest hmac)))

