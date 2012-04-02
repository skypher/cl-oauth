;;; Copyright (C) 2009 Leslie P. Polzer
;;; All rights reserved.
;;; See the file LICENSE for terms of use and distribution.

(in-package #:cl-user)

(defpackage :cl-oauth-asd
  (:use #:cl #:asdf))

(in-package :cl-oauth-asd)

(defsystem :cl-oauth
  :name "CL-OAuth"
  :description "Common Lisp OAuth implementation"
  :version "3"
  :maintainer "Leslie P. Polzer <polzer@gnu.org>"
  :licence "LLGPL"
  :components ((:static-file "cl-oauth.asd")
               (:module "src"
                        :components ((:file "package")
                                     (:module "util"
                                              :components ((:file "misc")
                                                           (:file "query-string"
                                                                  :depends-on ("misc"))
                                                           (:file "uri"
                                                                  :depends-on ("query-string")))
                                              :depends-on ("package"))
                                     (:module "core"
                                              :components ((:file "crypto")
                                                           (:file "request-adapter")
                                                           (:file "error-handling"
                                                                  :depends-on ("request-adapter"))
                                                           (:file "parameters"
                                                                  :depends-on ("request-adapter"))
                                                           (:file "signature")
                                                           (:file "tokens"
                                                                  :depends-on ("signature"))
                                                           (:file "consumer"
                                                                  :depends-on ("tokens" "parameters"
                                                                               "error-handling"))
                                                           (:file "service-provider"
                                                                  :depends-on ("tokens" "parameters"
                                                                               "error-handling")))
                                              :depends-on ("package" "util")))))
  :depends-on (:ironclad :cl-base64 :babel
               :closer-mop
               :alexandria :anaphora :f-underscore :split-sequence
               :trivial-garbage
               :drakma
               :puri :hunchentoot)
  :in-order-to ((test-op (load-op cl-oauth.tests))))

(defmethod operation-done-p ((op test-op) (c (eql (find-system :cl-oauth))))
  (values nil))

(defsystem :cl-oauth.tests
  :depends-on (:fiveam :cl-oauth)
  :pathname "test/"
  :components ((:file "package")
               (:module "core"
                        :components ((:file "request-adapter")
                                     (:file "parameters"
                                            :depends-on ("request-adapter"))
                                     (:file "signature"
                                            :depends-on ("request-adapter"))
                                     (:file "tokens")
                                     (:file "service-provider"
                                            :depends-on ("request-adapter")))
                        :depends-on ("package"))))
