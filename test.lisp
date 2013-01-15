;; copyright (c) 2012-2013 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.

;;
;; Testing Solr binding
;;
;;  To run the test, we need a Solr server running with example data
;;  loaded.   If you get Solr binary distribution, go down to example/
;;  directory and run the following command:
;;
;;   $ java -jar start.jar
;;
;;  This runs Solr example server in foreground.
;;
;;  If this is the first time, you need to populate the example database.
;;  In another shell window, go down to example/exampledocs/ directory
;;  and run the following command:
;;
;;   $ java -jar post.jar *.xml
;;
;;  See http://lucene.apache.org/solr/tutorial.html for the details.
;;  

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:require :tester)
  (cl:require :regexp2))

(cl:defpackage #:solr.test
  (:use #:cl #:excl #:util.test #:solr)
  (:export #:run-test #:run-test-solr-server))

(cl:in-package #:solr.test)

(defvar *solr-port* 8983
  "default port used by example")

(defvar *e* nil
  "with-solr macro binds this to a condition when socket-error occurs.
 Intended for diagnostics.")

(defmacro with-solr ((var uri) &body body)
  (let ((uri_ (gensym)) (e_ (gensym)))
    `(let* ((,uri_ ,uri)
            (,var (make-instance 'solr :uri ,uri_)))
       (declare (ignorable ,var))
       (setf *e* nil)
       (handler-case
           (progn ,@body)
         (socket-error (,e_)
           (setf *e* ,e_)
           (error "Can't connect to the Solr server at ~a: Maybe it is not running? (original socket error=~s)"
                  ,uri_ (slot-value ,e_ 'excl::identifier)))))))

;; run-test is run by (asdf:oos 'asdf:test-op :solr)
(defun run-test (&key (port *solr-port*))
  (with-tests (:name "solr")
    (with-solr (solr (format nil "http://localhost:~a/solr" port))
      ;; Response count
      (let ((r (solr-query solr)))
        (test '(17 0 10) (solr-result->response-count r)
              :multiple-values t)
        (test 10 (length (solr-result->doc-nodes r)))
        (test 10 (length (solr-result->doc-alist r))))
      (let ((r (solr-query solr :param-alist '((:rows . 20)))))
        (test '(17 0 17) (solr-result->response-count r)
              :multiple-values t))
      (let ((r (solr-query solr :param-alist '((:start 10 :rows 20)))))
        (test '(17 10 7) (solr-result->response-count r)
              :multiple-values t))
      )))

