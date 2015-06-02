all: ben.1 refman.html

REVISION=$(shell cd .. && dpkg-parsechangelog | sed -n "s/Version: //p")

ben.1:
	a2x --doctype manpage --format manpage $@.txt

refman.html:
	asciidoc -b html5 -a icons -a data-uri -a iconsdir=/etc/asciidoc/images/icons -a toc2 -a localrevision='$(REVISION)' $(@:.html=.txt)

.PHONY: ben.1 refman.html

clean:
	-rm -f refman.html ben.1
