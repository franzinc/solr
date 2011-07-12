This is a Solr binding for Allegro CL.  Solr is an open-source
freetext indexing/searching platform from the Apache Lucene project.
See the following URL for its details.

  http://lucene.apache.org/solr/

This package allows Allegro CL applications to communicate with a
running Solr server, add and delete documents, and run queries to
retrieve indexed records.

The package comes with a solr.asd file.  To use it just load :solr.

    (push #p"path/to/solr/source/directory" asdf:*central-registry*)
    (asdf:load-system :solr)

Accessing the database
----------------------

The Solr server should be running.  To access the server,
you need to create an instance of solr with the endpoint url.
For example, if the server is running on localhost:8983,
you can say:

    (defvar *solr* (make-instance 'solr :uri "http://localhost:8983/solr"))

This action itself doesn't actually connect to the database, but the
instance *solr* can be passed to other solr APIs to access to the
database.


Adding documents
----------------

To add a document, you can use solr-add and solr-add* API.

    (solr-add *solr* '((:id . 123) (:name . "foobar") (:author . "xyzzy")))

    (solr-add* *solr* list-of-records)

This adds the document with id=123, name="foobar" and author="xyzzy".
The document record is semantically an unordered collection of named
fields; you can pass an alist or a hashtable as a record.  Field names
are represented by keywords.  Field values are mapped as follows:

    Lisp numbers => Solr numbers
    Lisp date-time object => Solr Datetime
    Lisp strings => Solr text
    Lisp t and nil => Solr boolean

Non-empty Lisp lists can be used to represent set of values.

By default, solr-add and solr-add* do not commit the change.
solr-commit commits pending changes:

    (solr-commit *solr*)

Or, you can discard uncommitted changes by solr-rollback:

    (solr-rollback *solr*)

For convenience, solr-add and solr-add* accept a keyword argument
commit, that automatically commits the change before returning:

    (solr-add* *solr* list-of-records :commit t)

If you're adding large amount of documents, it is a good idea to send
a bunch of documents together before committing using solr-add*,
because it is much faster than adding documents one by one.

Occasionally you may want to call solr-optimize to optimize indices
for faster query performance:

    (solr-optimize *solr*)

Deleting documents
------------------

You can delete documents by listing document ids, or specifying
queries:

    (solr-delete *solr* :ids '(1 13 17))

    (solr-delete *solr* :queries '("author:Shiro"))

The deletion takes effects after committing.  solr-delete accepts the
commit keyword argument for autocommit.


Querying documents
------------------

solr-query does the query, and takes large number of keyword arguments
to customize the query.  See the Solr documentation for the full set
of features.  Here's an example of solr-query call:

    (solr-query *solr* :query "author:Shiro"
                :fields "id,name,author"
                :param-alist '((:rows . 100)))

It returns LXML, a S-expression representaton of XML.  It's up
to the caller to extract necessary information from the returned
LXML, but we provide a few convenience procedures for some
basic extraction.

    (solr-result->response-count lxml)

This returns three values: the total number of matching documents, the
starting record number, and the number of documents included in the
response.  Note that Solr does pagenation by default--if you don't
pass the :rows keyword it will only return the first 10 matching
records.  To retrieve other documents you need to pass the :start
keyword to solr-query.

Information on matching documents is in :doc XML elements.

    (solr-result->doc-nodes lxml)

This procedure extracts and returns the list of doc elements,
on which you can map to dig further information.

The extracted doc elements are still LXML.  The following procedure
further converts the field values to Lisp objects according to the
LXML attributes:

    (solr-result->doc-alist lxml)


Error handling
--------------

When the Solr server returns an error (e.g. invalid query format), a
condition solr-error is raised.  It contains the Solr response status
code, response headers and response body.  The response body is in
LXML.

When the API failed to communicate with Solr server (e.g. the server
isn't running), a socket-error condition is raised.


