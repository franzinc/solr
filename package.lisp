;; -*- mode: common-lisp -*-
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
           :solr-result->doc-nodes
           :solr-result->doc-alist
           ))
