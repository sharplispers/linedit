VERSION=0.6
NAME=linedit-$(VERSION)
LINK=linedit-latest.tar.gz

all:
	mkdir -p $(NAME) tarballs
	cp *.lisp *.c *.asd LICENSE Makefile README $(NAME)/
	tar -czvf $(NAME).tar.gz $(NAME)
	gpg -b -a $(NAME).tar.gz
	rm -rf $(NAME)
	ln -s $(NAME).tar.gz.asc $(LINK).asc
	ln -s $(NAME).tar.gz $(LINK)
	mv $(NAME).tar.gz $(NAME).tar.gz.asc $(LINK) $(LINK).asc tarballs/
