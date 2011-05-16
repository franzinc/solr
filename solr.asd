;;;; -*- mode: common-lisp -*-

(in-package :cl-user)

(require :aserve)
(require :datetime)
(require :net-xml-generator)

(asdf:defsystem solr
  :name "Solr"
  :author "Shiro Kawai"
  :version "0.1"

  :components ((:file "package")
               (:file "solr" :depends-on ("package")))
  )

  
  