# Boilerplate project makefile for Common-lisp.net
# May all your snapshots be orderly. ;)
#
# Per-project files needed: version.lisp-expr, release.txt
# Per-developer files needed: username.txt
#
# Following directory structure is assumed, relative to
# the directory of this Makefile (assumed to be in your
# source directory).
#
# ../public_html/
#

# Fill in you project name
PROJECT=linedit

# Release version number
VERSION=`sbcl --script linedit-version.lisp`
# Username
USERNAME=`cat username.txt`
# List of files included in release
FILES=`cat release.txt`

HTML=../public_html
DATE=`date +%F`
NAME=$(PROJECT)_$(VERSION)
LINK=$(PROJECT)_latest.tar.gz
SNAPSHOT=$(PROJECT)_$(DATE)
SNAPLINK=$(PROJECT)_latest-snapshot.tar.gz

CLNET=$(USERNAME)@common-lisp.net
CLNET_HOME=/project/$(PROJECT)
CLNET_HTML=$(CLNET_HOME)/public_html/

HTML_PERMS=ssh $(CLNET) "chgrp -R $(PROJECT) $(CLNET_HTML) && chmod -R ug+rw,o-w $(CLNET_HTML)*"
RSYNC=rsync -vlcrC 
RSYNC_HTML=$(RSYNC) $(HTML)/. $(CLNET):$(CLNET_HTML)

.PHONY: snapshot release public_html

all:
	@echo available targets: 
	@echo
	@echo "   public_html"
	@echo
	@echo "   snapshot: $(SNAPSHOT)"
	@echo "   release:  $(NAME)"
	@echo	

snapshot:
	mkdir -p $(SNAPSHOT)
	cp $(FILES) $(SNAPSHOT)/
	tar -czvf $(SNAPSHOT).tar.gz $(SNAPSHOT)
	rm -rf $(SNAPSHOT)/ $(SNAPSHOT).tar.gz.asc $(SNAPLINK) $(SNAPLINK).asc
	gpg -b -a $(SNAPSHOT).tar.gz
	ln -s $(SNAPSHOT).tar.gz.asc $(SNAPLINK).asc
	ln -s $(SNAPSHOT).tar.gz $(SNAPLINK)
	mv $(SNAPSHOT).tar.gz $(SNAPSHOT).tar.gz.asc $(SNAPLINK) $(SNAPLINK).asc $(HTML)/files/

release:
	mkdir -p $(NAME)
	cp $(FILES) $(NAME)/
	tar -czvf $(NAME).tar.gz $(NAME)
	rm -rf $(NAME)/ $(NAME).tar.gz.asc $(LINK) $(LINK).asc
	gpg -b -a $(NAME).tar.gz
	ln -s $(NAME).tar.gz.asc $(LINK).asc
	ln -s $(NAME).tar.gz $(LINK)
	mv $(NAME).tar.gz $(NAME).tar.gz.asc $(LINK) $(LINK).asc $(HTML)/files/
	echo $(VERSION) $(HTML)/version.txt

public_html:
	$(RSYNC_HTML) && $(HTML_PERMS)

TI_VERSION=1.3
TI=terminfo_$(TI_VERSION)
TI_LINK=terminfo_latest.tar.gz

terminfo:
	mkdir -p $(TI)
	cp terminfo.lisp terminfo.asd $(TI)/
	tar -czvf $(TI).tar.gz $(TI)
	gpg -b -a $(TI).tar.gz
	rm -rf $(TI)
	ln -s $(TI).tar.gz.asc $(TI_LINK).asc
	ln -s $(TI).tar.gz $(TI_LINK)
	mv $(TI).tar.gz $(TI).tar.gz.asc $(TI_LINK) $(TI_LINK).asc $(HTML)/files/
