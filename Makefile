VERSION=0.8
NAME=linedit-$(VERSION)
LINK=linedit-latest.tar.gz
FTP=../ftp

all:
	mkdir -p $(FTP) $(NAME)
	cp *.lisp *.c *.asd LICENSE ../doc/README $(NAME)/
	tar -czvf $(NAME).tar.gz $(NAME)
	gpg -b -a $(NAME).tar.gz
	rm -rf $(NAME)
	ln -s $(NAME).tar.gz.asc $(LINK).asc
	ln -s $(NAME).tar.gz $(LINK)
	mv $(NAME).tar.gz $(NAME).tar.gz.asc $(LINK) $(LINK).asc $(FTP)
	chown -R $(USER):linedit $(FTP)
	rsync -lgcrtC $(FTP)/. $(CLO_UI)@common-lisp.net:/project/linedit/ftp
