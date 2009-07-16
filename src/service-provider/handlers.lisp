
(in-package :oauth)

;;; TODO honor webapp uri prefix for weblocks applications.

(defvar *handlers* (make-hash-table :test #'eq))

(defun list-handlers ()
  (loop for name being the hash-keys of *handlers*
        collect name))

(defmacro define-handler ((name &key (prefix "/")) &body body)
  "Remove dispatchers associated with the symbol NAME  from the dispatch
  table, then add a newly created prefix dispatcher to the dispatch table.

  The URI prefix is built from NAME by lowercasing its symbol name
  and prepending PREFIX.
  
  NAME is defined as a global function using the scheme NAME-HANDLER
  to enable easy tracing."
  (let* ((handler-name (intern (concatenate 'string (symbol-name name) "-HANDLER")))
         (uri-prefix (concatenate 'string prefix (string-downcase (symbol-name name)))))
    (with-unique-names (old-dispatcher dispatcher)
      `(let ((,old-dispatcher (gethash ',name *handlers*))
             (,dispatcher (create-prefix-dispatcher ,uri-prefix ',handler-name)))
         (defun ,handler-name () ,@body)
         (setf *dispatch-table* (cons ,dispatcher (remove ,old-dispatcher *dispatch-table*))
               (gethash ',name *handlers*) ,dispatcher)))))

(define-handler (register-consumer)
  "Register a new consumer."
  (princ-to-string (register-consumer)))

(define-handler (get-request-token)
  "Hand out request tokens."
  (let ((callback (validate-request-token-request)))
    (unless callback
      (error "Not prepared for an OOB callback setup!"))
    (request-token-response (make-request-token :callback callback))))

(define-handler (get-user-authorization)
  "Let the user authorize the access token. [6.2.1]."
  (let ((request-token (validate-authorization-request)))
    (when t ; XXX authorize user here
      (setf (request-token-authorized-p request-token) t)
      (let ((callback-uri (request-token-callback-uri request-token)))
        (assert callback-uri)
        (hunchentoot:redirect (princ-to-string (finalize-callback-uri request-token)))))
    ;; NOTE: optionally notify the Consumer if the user refused authorization.
    ))

(define-handler (exchange-access-token)
  "Verify an access token and grant/deny access accordingly."
  "access token xch")

;; TODO: automatically define a handler that shows a page documenting
;; the other handlers. See section 4.2.

