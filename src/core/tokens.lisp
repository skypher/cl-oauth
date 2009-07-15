
(in-package :oauth)

;;; section 6.
;;;
;;;  OAuth Authentication is done in three steps:
;;;
;;;    1. The Consumer obtains an unauthorized Request Token.
;;;    2. The User authorizes the Request Token.
;;;    3. The Consumer exchanges the Request Token for an Access Token.
;;;

(defvar *registered-request-tokens* (make-hash-table :test #'equalp))
(defvar *registered-access-tokens* (make-hash-table :test #'equalp))

(defun create-request-token () nil)

