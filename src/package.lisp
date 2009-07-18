(defpackage #:cl-oauth
  (:nicknames #:oauth)
  (:use #:cl #:anaphora #:f-underscore)
  (:import-from #:hunchentoot
                #:create-prefix-dispatcher
                #:*dispatch-table*)
  (:import-from #:alexandria #:with-unique-names #:curry #:rcurry)
  (:import-from #:split-sequence #:split-sequence))

