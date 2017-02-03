;;;; -*- mode: common-lisp -*-

(in-package :cl-user)

(asdf:defsystem :solr
  :name "Solr"
  :author "Shiro Kawai / Franz Inc."
  :version "0.1"

  :components ((:file "package")
               (:file "solr" :depends-on ("package")))

  :in-order-to ((test-op (test-op :solr.test))))
  
(defmethod asdf:perform ((op asdf:test-op)
                         (system (eql (asdf:find-system :solr))))
  (asdf:load-system :solr.test)
  (let ((run-test (intern '#:run-test :solr.test)))
    (funcall run-test)))
