;; -*- mode: common-lisp -*-
;; copyright (c) 2011-2014 Franz Inc, Oakland, CA - All rights reserved.
;; This program and the accompanying materials are made available under the
;; terms of the Eclipse Public License v1.0 which accompanies this
;; distribution (see license.txt), and is available at
;;   http://www.eclipse.org/legal/epl-v10.html

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
