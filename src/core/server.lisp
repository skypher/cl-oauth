
(in-package :oauth)

(export '(start-server stop-server))


(defun start-server (&rest args)
  (apply #'start-weblocks :port 1888 args))

(defun stop-server ()
  (stop-weblocks))

