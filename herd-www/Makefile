PREFIX=$$HOME
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib/herd7
J=-j 8
JERD_OPT=-use-ocamlfind
BOOKS=aarch64 aarch64-mixed x86 linux
WWW_CATALOGUE=www/catalogue
HERD_CATALOGUE=../catalogue
OCB=ocamlbuild $(OCBOPT) $(JERD_OPT) -cflags -bin-annot,-w,A-4-9-29-45,-g -classic-display $(J)
PROG=jerd.js
BYTE=jerd.byte
WWW_PROG=www/$(PROG)
WWW_LIB=www/weblib
JSON_SPINES=$(foreach book,$(BOOKS),$(WWW_CATALOGUE)/$(book)/shelf.json)
CATINCLUDES=herd/libdir cat_includes $(foreach b,$(BOOKS),$(HERD_CATALOGUE)/$(b)/cats)
CATINCLUDESDEP=$(foreach d,$(CATINCLUDES),$(wildcard $(d)/*.*))
all: web

define make-json-shelf
$(WWW_CATALOGUE)/$1/shelf.json: $(WWW_CATALOGUE)/$1/
	cp catalogue_to_json.py $(WWW_CATALOGUE)/$1/
	cd $(WWW_CATALOGUE)/$1/ && (./catalogue_to_json.py > shelf.json || (rm -f shelf.json catalogue_to_json.py; exit 1))
	rm -f $(WWW_CATALOGUE)/$1/catalogue_to_json.py
endef

define make-book
$(WWW_CATALOGUE)/$1/:
	mkdir -p $(WWW_CATALOGUE)
	rsync -a --delete $(HERD_CATALOGUE)/$1/ $(WWW_CATALOGUE)/$1/

.PHONY:: $(WWW_CATALOGUE)/$1/
endef
tst:
	ocaml generate_names.ml $(CATINCLUDES)
weblib:
	@echo '** INSTALL ' $(WWW_LIB) && mkdir -p $(WWW_LIB) && cp ./cat.css $(WWW_LIB)
	@ocaml generate_names.ml $(CATINCLUDES) | while read f; do cp $$f $(WWW_LIB) && cat2html7 $(WWW_LIB)/$$(basename $$f) ; done

web: $(JSON_SPINES) $(PROG) weblib
	@cp -v $(PROG) www/$(PROG)

#js_of_ocaml --pretty --no-inline --debug-info $(BYTE)
$(PROG): byte
	js_of_ocaml $(BYTE)

catIncludes.ml: generate_includes.ml $(CATINCLUDESDEP)
	ocaml ./generate_includes.ml $(CATINCLUDES) > $@

byte native d.byte: catIncludes.ml
#	sh lib/handle402.sh
#	make -C herd version
	$(OCB) $(PROG:.js=.$@)

$(foreach book,$(BOOKS),$(eval $(call make-json-shelf,$(book))))
$(foreach book,$(BOOKS),$(eval $(call make-book,$(book))))

clean::
	-$(OCB) -clean

clean::
	/bin/rm -f *~
	/bin/rm -f $(PROG) $(BYTE) $(WWW_PROG) catIncludes.ml
	/bin/rm -f $(JSON_SHELVES)
	/bin/rm -rf $(WEB_LIB)

clean::

.PHONY:: byte native d.byte clean web
