(in-package #:cl-user)

(defpackage #:cl-mongo-system (:use #:cl #:asdf))

(in-package #:cl-mongo-system)

(defparameter *author-string* "Fons Haffmans
  <fons.haffmans@gmail.com>; Duong \"Yang\" Ha Nguyen
  <cmpitg@gmail.com>")

(defparameter *version-string* "0.7.1")

(asdf:defsystem cl-mongo-tim
  :name   "cl-mongo-tim"
  :author *author-string*
  :version *version-string*
  :licence "MIT"
  :description "A Common Lisp system to interact with MongoDB, a NoSQL
  DBS."
  :depends-on (:uuid
               :babel
               :bordeaux-threads
               :documentation-template
               :lisp-unit
               :parenscript
               :usocket)
  :serial t
  :components 
  ((:module "src"
            :serial t
            :components ((:file "packages")
                         (:file "octets")
                         (:file "pair")
                         (:file "encode-float")
                         (:file "bson-oid")
                         (:file "bson-binary")
                         (:file "bson-time")
                         (:file "bson-regex")
                         (:file "bson-code")
                         (:file "bson")
                         (:file "bson-decode")
                         (:file "bson-array")
                         (:file "document")
                         (:file "mongo-syntax")
                         (:file "java-script")
                         (:file "bson-encode-container")
                         (:file "protocol")
                         (:file "mongo")
                         (:file "db")
                         (:file "mem")
                         (:file "do-query")
                         (:file "doc")
                         (:file "map-reduce")
                         (:file "shell")))
   (:static-file "README.md")
   (:static-file "COPYING")))

(asdf:defsystem cl-mongo-test
  :name   "cl-mongo-tim"
  :author *author-string*
  :version *version-string*
  :licence "MIT"
  :description "testing cl-mongo"
  :depends-on (:cl-mongo)
  :serial t
  :components
  ((:module "test"
            :serial t
            :components ((:file "package")
                         (:file "test-utils")
                         (:file "regression")))))
