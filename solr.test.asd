(in-package :cl-user)

(asdf:defsystem :solr.test
  :name "Unit test for Solr"
  :author "Shiro Kawai / Franz Inc."
  :version "0.1"
  :components ((:file "test"))
  :depends-on (:solr))

