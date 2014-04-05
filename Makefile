DESTDIR   := /usr/local
ASDF_TREE := ~/quicklisp

gcal2org: cl-gcal2org.asd cl-gcal2org.lisp package.lisp
	buildapp --output $@ --asdf-tree $(ASDF_TREE) --load-system cl-gcal2org --entry 'gcal2org::main'

.PHONY: install clean
install: gcal2org
	install -m 555 $< $(DESTDIR)/bin/
clean:
	$(RM) gcal2org
