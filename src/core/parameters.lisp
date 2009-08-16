
(in-package :oauth)

(export '(parameter sort-parameters normalized-parameters))

;; the cache allows us to call NORMALIZED-PARAMETERS repeatedly
;; without excessive processing penalty.
(defvar *parameters-cache* (tg:make-weak-hash-table :test #'eq :weakness :key)
  "Per-request cache for parameters in OAuth requests.")

(defvar *signature-cache* (tg:make-weak-hash-table :test #'eq :weakness :key)
  ;; this is much more simple than maintaining multiple caches
  ;; for different parameter list flavors.
  "Per-request cache for signatures in OAuth requests.")

(defun sort-parameters (parameters)
  "Sort PARAMETERS according to the OAuth spec. This is a destructive operation."
  (assert (not (assoc "oauth_signature" parameters :test #'equal)))
  (sort parameters #'string< :key (lambda (x)
                                    "Sort by key and value."
                                    (concatenate 'string (princ-to-string (car x))
                                                 (princ-to-string (cdr x))))))

(defun normalized-parameters (&key remove-duplicates-p)
  "Collect request parameters and remove those excluded by the standard. See 9.1.1.
  Note: REMOVE-DUPLICATES-P has no effect right now."
  (declare (ignorable remove-duplicates-p))
  (or (gethash (request) *parameters-cache*)
      (let ((parameters (append (remove "realm" (auth-parameters)
                                        :key #'car :test #'equalp) ; TODO: http auth header parameters
                                (post-parameters)
                                (get-parameters))))
        ;; save the signature, we might need it later
        (setf (gethash (request) *signature-cache*)
              (cdr (assoc "oauth_signature" parameters :test #'equal)))
        (let* ((parameters (remove "oauth_signature" parameters
                                   :key #'car :test #'equal))
               (sorted-parameters (sort-parameters parameters)))
          (setf (gethash (request) *parameters-cache*) sorted-parameters)
          sorted-parameters
          #+(or) ; disabled for now because it makes caching slightly more complex.
                 ; we just don't support elimination of duplicates right now.
          (if remove-duplicates-p
            (remove-duplicates sorted-parameters :key #'car :test #'string-equal :from-end t)
            sorted-parameters)))))

(defun parameter (name &key (test #'equal))
  "Note: OAuth parameters are case-sensitive per section 5.
  The case of user-supplied parameters is not restricted."
  (cdr (assoc name (normalized-parameters) :test test)))

(defun oauth-parameter-p (parameter)
  "Return T if PARAMETER starts with \"oauth_\". PARAMETER is a
string denoting the parameter name."
  (equal
    (subseq (car (ensure-list parameter)) 0 (min 6 (length parameter)))
    "oauth_" ))

(defun remove-oauth-parameters (parameters)
  (remove-if #'oauth-parameter-p parameters :key #'car))

