(asdf:oos 'asdf:load-op 'cl-who)

(in-package :oauth)

;;; TODO honor webapp uri prefix for weblocks applications.

(defvar *handlers* (make-hash-table :test #'eq))

(defun list-handlers ()
  (loop for name being the hash-keys of *handlers*
        collect name))

(defvar *debug-on-error* nil)

(defmacro define-handler ((name &key (prefix "/") (http-error-handler #'default-error-handler)) &body body)
  "Remove dispatchers associated with the symbol NAME  from the dispatch
  table, then add a newly created prefix dispatcher to the dispatch table.

  The URI prefix is built from NAME by lowercasing its symbol name
  and prepending PREFIX.
  
  NAME is defined as a global function using the scheme NAME-HANDLER
  to enable easy tracing."
  (let* ((handler-name (intern (concatenate 'string (symbol-name name) "-HANDLER")))
         (uri-prefix (concatenate 'string prefix (string-downcase (symbol-name name)))))
    (with-unique-names (old-dispatcher dispatcher)
      (multiple-value-bind (body declarations) (alexandria:parse-body body)
        `(let ((,old-dispatcher (gethash ',name *handlers*))
               (,dispatcher (create-prefix-dispatcher ,uri-prefix ',handler-name)))
           (defun ,handler-name ()
             ,@declarations
             (handler-bind ((http-error ,http-error-handler)
                            (error (lambda (c)
                                     (if *debug-on-error*
                                       (invoke-debugger c)
                                       (format t "error: ~A~%" c)))))
               ,@body))
           (setf *dispatch-table* (cons ,dispatcher (remove ,old-dispatcher *dispatch-table*))
                 (gethash ',name *handlers*) ,dispatcher))))))

(define-handler (register-consumer)
  "Register a new consumer."
  (cl-who:escape-string (princ-to-string (register-token (make-consumer-token)))))

(define-handler (get-request-token)
  "Hand out request tokens."
  (let ((request-token (validate-request-token-request)))
    (request-token-response request-token)))

(define-handler (get-user-authorization)
  "Let the user authorize the access token. [6.2.1]."
  (protocol-assert (eq (request-method) :get)) ; [6.2.1]
  (let ((request-token (get-supplied-request-token)))
    (when t ; XXX obtain user permission here
      (setf (request-token-authorized-p request-token) t)
      ;; now notify the Consumer that the request token has been authorized.
      (let ((callback-uri (request-token-callback-uri request-token)))
        (cond
          ((eq *protocol-version* :1.0)
           ;; callback uri is optional in 1.0; you might want to employ
           ;; some other means to construct it.
           (hunchentoot:abort-request-handler "Authorization complete."))
          (t
           (protocol-assert callback-uri)
           (hunchentoot:redirect (princ-to-string (finalize-callback-uri request-token)))))))
    ;; only reached when authorization failed

    ;; NOTE: optionally notify the Consumer if the user refused authorization.
    ))

(define-handler (get-access-token)
  "Get an access token from a previously issued and authorized request token."
  (let ((access-token (validate-access-token-request)))
    (princ-to-string access-token)))

(define-handler (protected-resource)
  (validate-access-token)
  "All your base are belong to us.")

;; TODO: automatically define a handler that shows a page documenting
;; the location of the other handlers. See section 4.2.

