
(in-package :oauth)

(export '(url-encode))

(defvar +utf-8+ (flexi-streams:make-external-format :utf8 :eol-style :lf))

;; this function is taken from Hunchentoot but modified to
;; satisfy the OAuth spec demands.
(defun url-encode (input &optional (external-format +utf-8+))
  "URL-encodes INPUT according to the percent encoding rules of
  RFC5849 (section 3.6).  If a string is passed as INPUT, it is
  encoded using the external format EXTERNAL-FORMAT.  If a vector of
  bytes is passed, the values are used verbatim."
  (with-output-to-string (s)
    (loop for octet across (etypecase input
                             (string
                              (flexi-streams:string-to-octets input :external-format external-format))
                             ((or (array (integer) (*))
                                  (array (unsigned-byte 8) (*)))
                              input)
                             (null
                              #()))
          for char = (code-char octet)
          do (if (or (char<= #\0 char #\9)
                     (char<= #\a char #\z)
                     (char<= #\A char #\Z)
                     (find char "-_.~" :test #'char=))
                 (write-char char s)
                 (format s "%~2,'0x" octet)))))

(defmacro upgrade-vector (vector new-type &key converter)
  "Returns a vector with the same length and the same elements as
VECTOR \(a variable holding a vector) but having element type
NEW-TYPE.  If CONVERTER is not NIL, it should designate a function
which will be applied to each element of VECTOR before the result is
stored in the new vector.  The resulting vector will have a fill
pointer set to its end.

The macro also uses SETQ to store the new vector in VECTOR."
  `(setq ,vector
         (loop with length = (length ,vector)
               with new-vector = (make-array length
                                             :element-type ,new-type
                                             :fill-pointer length)
               for i below length
               do (setf (aref new-vector i) ,(if converter
                                               `(funcall ,converter (aref ,vector i))
                                               `(aref ,vector i)))
               finally (return new-vector))))

;;; this function is taken from Hunchentoot 1.1.0 without effective modification
(defun url-decode (string &optional (external-format +utf-8+))
  "Decodes a URL-encoded STRING which is assumed to be encoded using
the external format EXTERNAL-FORMAT."
  (when (zerop (length string))
    (return-from url-decode ""))
  (let ((vector (make-array (length string) :element-type '(unsigned-byte 8) :fill-pointer 0))
        (i 0)
        unicodep)
    (loop
      (unless (< i (length string))
        (return))
      (let ((char (aref string i)))
       (labels ((decode-hex (length)
                  (prog1
                      (parse-integer string :start i :end (+ i length) :radix 16)
                    (incf i length)))
                (push-integer (integer)
                  (vector-push integer vector))
                (peek ()
                  (aref string i))
                (advance ()
                  (setq char (peek))
                  (incf i)))
         (cond
          ((char= #\% char)
           (advance)
           (cond
            ((char= #\u (peek))
             (unless unicodep
               (setq unicodep t)
               (upgrade-vector vector '(integer 0 65535)))
             (advance)
             (push-integer (decode-hex 4)))
            (t
             (push-integer (decode-hex 2)))))
          (t
           (push-integer (char-code (case char
                                      ((#\+) #\Space)
                                      (otherwise char))))
           (advance))))))
    (cond (unicodep
           (upgrade-vector vector 'character :converter #'code-char))
          (t (flexi-streams:octets-to-string vector :external-format external-format)))))


(defmethod normalize-uri ((uri string))
  (normalize-uri (puri:parse-uri uri)))

(defmethod normalize-uri ((uri puri:uri))
  "9.1.2"
  (let ((*print-case* :downcase) ; verify that this works!!
        (scheme (puri:uri-scheme uri))
        (host (puri:uri-host uri))
        (port (puri:uri-port uri))
        (path (puri:uri-path uri)))
    (values
      (concatenate 'string
        (string-downcase (princ-to-string scheme))
        "://"
        (string-downcase host)
        (cond
          ((null port)
           "")
          ((and (eq scheme :http) (eql port 80))
           "")
          ((and (eq scheme :https) (eql port 443))
           "")
          (t
           (concatenate 'string ":" (princ-to-string port))))
        path)
      (awhen (puri:uri-query uri)
        (query-string->alist it)))))

