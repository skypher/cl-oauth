
(in-package :oauth)

;;; section 6.
;;;
;;;  OAuth Authentication is done in three steps:
;;;
;;;    1. The Consumer obtains an unauthorized Request Token.
;;;    2. The User authorizes the Request Token.
;;;    3. The Consumer exchanges the Request Token for an Access Token.
;;;

;;; TODO: token registry GC

;;; default token values
(let ((random-state (make-random-state t)))
  (defun random-key ()
    (format nil "~36,25,'0r" (random (expt 36 25) random-state)))

  (defun random-secret ()
    (format nil "~36,25,'0r" (random (expt 36 25) random-state)))

  (defun random-verification-code ()
    (format nil "~36,25,'0r" (random (expt 36 25) random-state))))


;;; token base class
(defclass token ()
  ((key :type string
        :reader token-key
        :initarg :key
        :initform (random-key))
   (secret :type string
           :reader token-secret
           :initarg :secret
           :initform (random-secret))
   (user-data :type list
              :accessor token-user-data
              :initarg :user-data
              :initform nil
              :documentation "Application-specific data associated
              with this token; an alist.")))

(defmethod print-object ((obj token) stream)
  "Faking STRUCT-like output. It would probably be better to use
  the pretty printer; the code for sb-kernel::%default-structure-pretty-print
  will be a useful template."
  (print-unreadable-object (obj stream :type t :identity (not *print-pretty*))
    (loop for slotname in (mapcar #'c2mop:slot-definition-name
                                  (c2mop:class-slots (class-of obj)))
          do (progn
               (terpri stream)
               (write "  " :stream stream :escape nil)
               (prin1 (intern (symbol-name slotname) :keyword) stream)
               (write " " :stream stream :escape nil)
               (prin1 (if (slot-boundp obj slotname)
                        (slot-value obj slotname)
                        "(unbound)")
                      stream)))))


;;; consumer tokens
(defclass consumer-token (token)
  ((last-timestamp :type integer
                   :accessor consumer-token-last-timestamp
                   :initform 0)))

(defun make-consumer-token (&rest args)
  (apply #'make-instance 'consumer-token args))


(defclass consumer-ref-mixin ()
  ((consumer :type consumer-token
             :accessor token-consumer
             :initarg :consumer
             :documentation "The Consumer that originally requested this
             token."))
  (:documentation "Mixin for classes that refer to a consumer."))


;;; request tokens
(defclass request-token (token consumer-ref-mixin)
  ((callback-uri :type (or puri:uri null)
                 :reader request-token-callback-uri
                 :initarg :callback-uri
                 :initform nil
                 :documentation "Callback URI for this request token.
                 NIL means oob.")
   (verification-code :type (or string null)
                      :accessor request-token-verification-code
                      :initarg :verification-code
                      :initform (random-verification-code)
                      :documentation "Might be NIL for OAuth 1.0")
   (authorized-p :type boolean
                 :accessor request-token-authorized-p
                 :initform nil)))

(defun make-request-token (&rest args)
  (apply #'make-instance 'request-token args))


;;; access tokens
(defclass access-token (token consumer-ref-mixin)
  ((session-handle :type (or string null)
                   :reader access-token-session-handle
                   :initarg :session-handle
                   :initform nil)
   (expires :type (or integer null)
            :reader access-token-expires
            :initarg :expires
            :initform nil
            :documentation "Universal time when this token expires.")
   (authorization-expires
     :type (or integer null)
     :reader access-token-authorization-expires
     :initarg :authorization-expires
     :initform nil
     :documentation "Universal time when this token's session expires.")
   (origin-uri
     :type (or puri:uri string null)
     :reader access-token-origin-uri
     :initarg :origin-uri
     :initform nil
     :documentation "URI this access token has been obtained from.
                     Needed for refresh.")))


(defun make-access-token (&rest args)
  (apply #'make-instance 'access-token args))

(defun access-token-expired-p (access-token)
  (and (access-token-session-handle access-token)
       (or (aand (access-token-expires access-token)
                 (> (get-universal-time) it))
           (aand (access-token-authorization-expires access-token)
                 (> (get-universal-time) it)))))

