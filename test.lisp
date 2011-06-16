;;
;; Testing Solr binding
;;
;;  To run the test, we need a Solr server running and example data
;;  is loaded.   If you get Solr binary distribution, go down to example/
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

(defvar *solr-port* 8983) ; default port used by example

(defun te (expected expr)
  (test expected expr :test #'equal))

(defmacro with-solr ((var uri) &body body)
  (let ((uri_ (gensym)) (e_ (gensym)))
    `(let* ((,uri_ ,uri)
            (,var (make-instance 'solr :uri ,uri_)))
       (declare (ignorable ,var))
       (handler-case
           (progn ,@body)
         (socket-error (,e_)
           (setf *e* ,e_)
           (error "Can't connect to the Solr server at ~a: Maybe it is not running? (original socket error=~s)"
                  ,uri_ (slot-value ,e_ 'excl::identifier)))))))

(defun run-test (&key (port *solr-port*))
  (with-tests (:name "solr")
    (with-solr (solr (format nil "http://localhost:~a/solr" port))
      (te (solr-query solr) 1)
      )))

;;