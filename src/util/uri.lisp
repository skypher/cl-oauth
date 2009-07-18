
(in-package :oauth)

(export '(url-encode))

;; TODO: format must always be UTF8 in OAuth context.
(defun url-encode (string &optional (external-format hunchentoot:*hunchentoot-default-external-format*))
  "URL-encodes a string using the external format EXTERNAL-FORMAT."
  (with-output-to-string (s)
    (loop for c across string
          for index from 0
          do (cond ((or (char<= #\0 c #\9)
                        (char<= #\a c #\z)
                        (char<= #\A c #\Z)
                        ;; note that there's no comma in there - because of cookies
                        (find c "-_.~" :test #'char=))
                     (write-char c s))
                   (t (loop for octet across (babel:string-to-octets string
                                                                     :start index
                                                                     :end (1+ index)
                                                                     :external-format external-format)
                            do (format s "%~2,'0x" octet)))))))

(defmethod normalize-request-uri ((uri string))
  (normalize-request-uri (puri:parse-uri uri)))

(defmethod normalize-request-uri ((uri puri:uri))
  "9.1.2"
  (let ((*print-case* :downcase) ; verify that this works!!
        (scheme (puri:uri-scheme uri))
        (host (puri:uri-host uri))
        (port (puri:uri-port uri))
        (path (puri:uri-path uri)))
    (concatenate 'string (string-downcase (princ-to-string scheme))
                         "://"
                         (string-downcase host)
                         (cond
                           ((and (eq scheme :http) (eql port 80))
                            "")
                           ((and (eq scheme :https) (eql port 443))
                            "")
                           (t
                            (concatenate 'string ":" (princ-to-string port))))
                         path)))

