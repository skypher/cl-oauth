(defpackage #:oauth
  (:use #:cl #:weblocks #:anaphora #:f-underscore)
  (:import-from #:hunchentoot
                #:create-prefix-dispatcher
                #:*dispatch-table*
                #:*request*)
  (:import-from #:alexandria #:with-unique-names #:curry #:rcurry))

