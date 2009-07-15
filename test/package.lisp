(defpackage #:oauth-test
  (:use #:cl #:oauth #:5am) 
  (:import-from #:alexandria #:with-unique-names #:curry #:rcurry))

(in-package :oauth-test)

(def-suite oauth)

