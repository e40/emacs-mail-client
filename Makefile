
MHE = 8.6

.PHONY: default clean tags

default: clean tags
	cd mh-e-$(MHE); make

clean:
	rm -f *.elc TAGS

tags:
	rm -f TAGS
	find . -name '*.el' | xargs etags -a
