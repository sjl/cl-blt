.PHONY: vendor docs pubdocs swig

sourcefiles = $(shell ffind --full-path --literal .lisp)
docfiles = $(shell ls docs/*.markdown)
apidocs = $(shell ls docs/*reference*.markdown)

# Vendor ----------------------------------------------------------------------
vendor/quickutils.lisp: vendor/make-quickutils.lisp
	cd vendor && sbcl --noinform --load make-quickutils.lisp  --eval '(quit)'

vendor: vendor/quickutils.lisp

# SWIG ------------------------------------------------------------------------
src/low-level/bearlibterminal.lisp: src/low-level/bearlibterminal.swig src/low-level/include/BearLibTerminal.h package.lisp
	swig -cffi -module bearlibterminal src/low-level/bearlibterminal.swig

swig: src/low-level/bearlibterminal.lisp 

# Documentation ---------------------------------------------------------------
$(apidocs): $(sourcefiles)
	sbcl --noinform --load docs/api.lisp  --eval '(quit)'

docs/build/index.html: $(docfiles) $(apidocs) docs/title
	cd docs && ~/.virtualenvs/d/bin/d

docs: docs/build/index.html

pubdocs: docs
	hg -R ~/src/docs.stevelosh.com pull -u
	rsync --delete -a ./docs/build/ ~/src/docs.stevelosh.com/cl-blt
	hg -R ~/src/docs.stevelosh.com commit -Am 'cl-blt: Update site.'
	hg -R ~/src/docs.stevelosh.com push
