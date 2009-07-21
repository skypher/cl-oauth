
(in-package :oauth)

(export '(start-server stop-server))

(defvar *server* nil)

(defun start-server (&rest acceptor-args)
  (if *server*
    (warn "Server already started, doing nothing.")
    (hunchentoot:start (apply #'make-instance 'hunchentoot:acceptor acceptor-args))))

(defun stop-server ()
  (when *server*
    (hunchentoot:stop *server*)))

