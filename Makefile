
VERSION = 27.1

.PHONY: default clean tags

default: clean tags
	rm -f *.elc; \
	cd mh-e-$(VERSION); \
	rm -f *.elc; \
	emacs -q -batch -f batch-byte-compile *.el

clean:
	rm -f *.elc */*.elc TAGS

tags:
	rm -f TAGS
	etags *.el mh-e-$(VERSION)/*.el
