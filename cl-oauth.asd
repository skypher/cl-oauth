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
  :version "2"
  :maintainer "Leslie P. Polzer <polzer@gnu.org>"
  :licence "LLGPL"
  :components ((:static-file "cl-oauth.asd")
               (:module "src"
                        :components ((:file "package")
                                     (:module "util"
                                              :components ((:file "misc")
                                                           (:file "uri")
                                                           (:file "query-string"
                                                                  :depends-on ("misc")))
                                              :depends-on ("package"))
                                     (:module "core"
                                              :components ((:file "crypto")
                                                           (:file "request-adapter")
                                                           (:file "parameters"
                                                                  :depends-on ("request-adapter"))
                                                           (:file "signature")
                                                           (:file "tokens"
                                                                  :depends-on ("signature"))
                                                           (:file "service-provider"
                                                                  :depends-on ("tokens")))
                                              :depends-on ("package" "util"))
                                     (:module "consumer"
                                              :components ()
                                              :depends-on ("package" "core"))))
               (:module "test"
                        :components ((:file "package")
                                     (:module "core"
                                              :components ((:file "request-adapter")
                                                           (:file "parameters"
                                                                  :depends-on ("request-adapter"))
                                                           (:file "signature"
                                                                  :depends-on ("request-adapter")))
                                              :depends-on ("package")))))
  :depends-on (:ironclad :cl-base64 :babel
               :closer-mop
               :alexandria :anaphora :f-underscore :split-sequence
               :trivial-garbage
               :fiveam
               :puri :hunchentoot))

(defmethod perform ((o asdf:test-op) (c (eql (find-system :cl-oauth))))
  (funcall (intern "RUN!" :5am)
           (intern "OAUTH" :oauth-test)))

