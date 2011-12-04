;; -*- mode: common-lisp -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :aserve)
  (require :datetime)
  (require :net-xml-generator))

(defpackage :solr
  (:use :cl :excl
        :util.date-time
        :net.aserve.client
        :net.xml.generator
        :net.xml.parser)
  (:export :solr
           :solr-error
           :solr-add
           :solr-add*
           :solr-commit
           :solr-optimize
           :solr-rollback
           :solr-delete
           :solr-query
           :solr-result->response-count
           :solr-result->doc-nodes
           :solr-result->doc-alist
           ))
