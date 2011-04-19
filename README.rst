JSON Pairs
==========

* Constructing:

    ``(mongo-pair key value)``
    ``(m-pair key value)``

JSON Documents
==============

* Constructing:

    ``(mongo-document mongo-pair-1 mongo-pair-2 ...)``
    ``(m-doc mongo-pair-1 mongo-pair-2 ...)``

* Inserting a pair:

    ``(mongo-document.insert a-mongo-pair a-mongo-document)``
    ``(m-doc.insert a-mongo-pair a-mongo-document)``
