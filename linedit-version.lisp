;;; Script used from the Makefile only
(require :asdf)
(write-line (asdf:component-version (asdf:find-system :linedit)))
