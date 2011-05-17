;; -*- mode: common-lisp -*-
(in-package :solr)

(eval-when (compile eval)
  (setq *readtable* (excl:named-readtable :xml)))

;; Solr API
;;
;; Example usage:
;;
;;   (defvar *solr* (make-instance 'solr :uri "http://localhost:8983/solr"))
;;
;;   (solr-add *solr* '((id . 123) (name . "foobar") (author . "xyzzy")))
;;
;;   (solr-commit *solr*)
;;
;;   (solr-query *solr* :query "name:foobar")
;;
;;   (solr-delete *solr* :ids '(123))
;;
;; Condition(s):
;;
;;   solr-error 
;;
;;      When Solr server returns an error (response whose status is not 200),
;;      this condition is thrown.   Slots are:
;;
;;       status-code      - the response status, e.g. 400
;;       response-headers - assoc list of parsed response headers
;;       response-body    - LXML format of response body.
;;
;; Solr record representation:
;;
;;   solr-add and solr-add* takes a record to represent a document.
;;   Semantically, a solr record is a collection of named fields.
;;   In the lisp world, it can be represented as an assoc list or
;;   a hashtable.
;;   Field names are represented by keywords.
;;   Field values mapping:
;;     Multiple values in Solr record are represented in Lisp list.
;;     Numbers are mapped to Lisp numbers.
;;     Datetime is mapped to date-time class.
;;     Text is mapped to Lisp strings.
;;     Boolean value is mapped to Lisp keyword :true and :false
;;      (We avoid using nil, for it is indistinguishable from empty set).

;;;
;;; Connection representation and condition
;;;

;; Public
(defclass solr ()
  ((uri         :initarg :uri
                :reader solr-uri
                :documentation "URI of Solr REST API endpoint, e.g. http://localhost:8983/solr")
   )
  (:documentation "An object holding Solr endpoint"))

(defmethod print-object ((solr solr) stream)
  (print-unreadable-object (solr stream :type t)
    (princ (solr-uri solr) stream)))

;; Public
(define-condition solr-error (error)
  ((status-code      :initarg :status-code)
   (response-headers :initarg :response-headers)
   (response-body    :initarg :response-body)))

;; a utility macro
(defmacro xml->string (&body body)
  (let ((s (gensym)))
    `(with-output-to-string (,s)
       (with-xml-generation (,s)
         ,@body))))

;;;
;;; Updating
;;;

;; API
(defmethod solr-add ((solr solr) doc &key (commit nil)
                                          (overwrite t))
  "Add a new document to the Solr pointed by SOLR.
DOC can be a hashtable or an assoc list.
If COMMIT is true, the record is committed immediately.
If OVERWRITE is true, an existing record with the same key field will be
replaced with DOC, if any.

Example:
  (solr-add solr '((id . 1234) (name . \"foo\")
                   (text . \"Lorem ipsum dolor sit amet, consectetur
   adipisicing elit, sed do eiusmod tempor incididunt ut labore et
   dolore magna aliqua.\"))
                 :commit t)

On success, returns LXML representation of the Solr server response.
"
  (let ((msg (xml->string
              ^((add @overwrite (xbool overwrite))
                ^(doc (render-record doc))))))
    (post-request solr msg `((commit . ,(xbool commit))))))

;; API
(defmethod solr-add* ((solr solr) docs &key (commit nil)
                                            (overwrite t))
  "Add a new documents to the Solr pointed by SOLR.
DOCS is a list of hashtables or assoc lists.
If COMMIT is true, the record is committed immediately.
If OVERWRITE is true, an existing record with the same key field will be
replaced with DOC, if any.
On success, returns LXML representation of the Solr server response."
  (let ((msg (xml->string
              ^((add @overwrite (xbool overwrite))
                (dolist (doc docs)
                  ^(doc (render-record doc)))))))
    (post-request solr msg `((commit . ,(xbool commit))))))

;; API
(defmethod solr-commit ((solr solr) &key (wait-flush t)
                                         (wait-searcher t)
                                         (expunge-deletes nil))
  "Send COMMIT command.
WAIT-FLUSH controls whether the request waits after the data is written
to the disk; default is T.
WAIT-SEARCHER controls whether the request watis until searcher objects
to be warmed for use; default is T.
EXPUNGE-DELETS controls whether sergments with deletes are merged away;
default is NIL.
On success, returns LXML representation of the Solr server response."
  (let ((msg (xml->string
              ^((commit @waitFlush (xbool wait-flush)
                        @waitSearcher (xbool wait-searcher)
                        @expungeDeletes (xbool expunge-deletes))))))
    (post-request solr msg)))

;; API
(defmethod solr-optimize ((solr solr) &key (wait-flush t)
                                           (wait-searcher t)
                                           (max-segments 1))
  "Send OPTIMIZE command.
WAIT-FLUSH controls whether the request waits after the data is written
to the disk; default is T.
WAIT-SEARCHER controls whether the request watis until searcher objects
to be warmed for use; default is T.
MAX-SEGMENTS sets the maximum number of segments to optimize down;
default is 1.
On success, returns LXML representation of the Solr server response."
  (let ((msg (xml->string
              ^((optimize @waitFlush (xbool wait-flush)
                          @waitSearcher (xbool wait-searcher)
                          @maxSegments max-segments)))))
    (post-request solr msg)))

;; API
(defmethod solr-rollback ((solr solr))
  "Send ROLLBACK command.
On success, returns LXML representation of the Solr server response."
  (post-request solr "<rollback/>"))

;; API
(defmethod solr-delete ((solr solr) &key (ids nil) (queries nil) (commit nil))
  "Deletes the documents matching given IDs or queries.
IDS takes a list of numeric ids; documents with matching uniqueKey field
defined in schema are deleted.
QUERIES takes a list of queies in strings.  A simple one is <field>:<value>,
such as \"author:Shiro\".
If COMMIT is T, deletes are committed immediately.
On success, returns LXML representation of the Solr server response."
  (let ((msg (xml->string
               ^(delete
                 (dolist (id ids) ^(id @id))
                 (dolist (q queries) ^(query @q))))))
    (post-request solr msg `((commit . ,(xbool commit))))))

;;;
;;; Query
;;;

;; API
(defmethod solr-query ((solr solr) &key (query "*:*")
                                        (fields "*")
                                        (search-name "select")
                                        (score t)
                                        (sort nil)
                                        (param-alist nil))
  "Searches documents according to the given QUERY.
Returns LXML representation of the Solr server response.
FIELDS specifies which fields to be included in the results; the default
is \"*\".
SEARCH-NAME names the name of the customized search; if omitted,
the default \"select\" search is used."
  (let ((uri (format nil "~a/~a" (solr-uri solr) search-name))
        (q `((q . ,query)
             (fields . ,fields)
             (score . ,(xbool score))
             ,@(if sort `((sort . ,sort)))
             ,@param-alist)))
    (multiple-value-bind (body status headers)
        (do-http-request/retry uri
          :method :get :query q :external-format :utf-8)
      (parse-response body status headers))))

;;;
;;; Some utilities
;;;

;; Retry if we get EADDRNOTAVAIL - it means we've consumed local ports
;; faster than the system reclaims it, so it is reasonable to retry
;; 
(defun do-http-request/retry (uri &rest keys)
  (loop
     (handler-case
         (return (apply #'do-http-request uri keys))
       (socket-error (condition)
         (if* (eq (stream-error-identifier condition) :address-not-available)
           then (sleep 0.01)
           else (error condition))))))

;; Common procedure for request-response
(defun post-request (solr body &optional query-alist)
  (multiple-value-bind (body status headers)
      (do-http-request/retry (update-endpoint solr query-alist)
        :method :post :content body :content-type "text/xml"
        :external-format :utf-8)
    (parse-response body status headers)))

;; Parse response
(defun parse-response (body status headers)
  (let ((lxml (parse-xml body)))
    (when (not (eql status 200))
      (error 'solr-error :status-code status :response-headers headers
             :response-body lxml))
    lxml))

;; Some Solr POST message can take optional parameters via url query string.
;; We can't use :query argument of do-http-request, for we have to use
;; both url query string and POST message body, while do-http-request
;; assumes the query string to be the POST message body.
(defun update-endpoint (solr &optional query-params)
  (let ((uri (solr-uri solr)))
    (if query-params
        (format nil "~a/update?~a" uri (net.aserve:query-to-form-urlencoded query-params :external-format :utf-8))
        (format nil "~a/update" uri))))

;; Rendering record to xml.  Needs to be called within the dynamic
;; extent of with-xml-generation.
(defun render-record (rec)
  (if* (hash-table-p rec)
    then (maphash #'render-field rec)
    else (loop for (key . val) in rec do (render-field key val))))

(defun render-field (key val)
  (if* (listp val)
    then (dolist (v val) (render-field key v))
    else ^((field @name key) @(render-value val))))

(defun render-value (val)
  (etypecase val
    (number val)
    (string val)
    (symbol val)
    (date-time
     (with-output-to-string (s)
       (let ((*date-time-fmt* "%Y-%m-%dT%H:%M:%SZ"))
         ;; ensure we use UTC
         (princ (ut-to-date-time (date-time-to-ut val) :time-zone 0) s))))))

(defun xbool (val) (if val 'true 'false))
