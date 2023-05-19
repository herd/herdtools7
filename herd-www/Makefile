PREFIX=$$HOME
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib/herd7
J=-j 8
JERD_OPT=-use-ocamlfind
BOOKS=aarch64  aarch64-mixed aarch64-MTE aarch64-MTE-mixed aarch64-VMSA  aarch64-ETS2 aarch64-faults x86 linux
#BOOKS=aarch64-ptw #Add some day.
WWW_CATALOGUE=www/catalogue
HERD_CATALOGUE=../catalogue
PROG=jerd.js
WWW_PROG=www/$(PROG)
WWW_LIB=www/weblib
JSON_SHELVES=$(foreach book,$(BOOKS),$(WWW_CATALOGUE)/$(book)/shelf.json)
CATINCLUDES=../herd/libdir linux $(foreach b,$(BOOKS),$(HERD_CATALOGUE)/$(b)/cats)
CATINCLUDESDEP=$(foreach d,$(CATINCLUDES),$(wildcard $(d)/*.*))
DUNETGT=../_build/default/herd-www
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
	rsync -a --copy-unsafe-links --delete $(HERD_CATALOGUE)/$1/ $(WWW_CATALOGUE)/$1/

.PHONY:: $(WWW_CATALOGUE)/$1/
endef
tst:
	ocaml generate_names.ml $(CATINCLUDES)
weblib:
	@echo '** INSTALL ' $(WWW_LIB) && mkdir -p $(WWW_LIB) && cp ./cat.css $(WWW_LIB)
	@ocaml generate_names.ml $(CATINCLUDES) | while read f; do cp $$f $(WWW_LIB) && cat2html7 $(WWW_LIB)/$$(basename $$f) ; done

web: $(JSON_SHELVES) $(PROG) weblib
	@cp -v $(PROG) www/$(PROG)

$(PROG): catIncludes.ml
	( cd .. && dune build --release @jerd )
	cp $(DUNETGT)/jerd.bc.js $(PROG) && chmod u+w $(PROG)

catIncludes.ml: generate_includes.ml $(CATINCLUDESDEP)
	ocaml ./generate_includes.ml $(CATINCLUDES) > $@

$(foreach book,$(BOOKS),$(eval $(call make-json-shelf,$(book))))
$(foreach book,$(BOOKS),$(eval $(call make-book,$(book))))

clean::
	/bin/rm -f *~
	/bin/rm -f $(PROG) $(WWW_PROG) catIncludes.ml
	/bin/rm -f $(JSON_SHELVES)
	/bin/rm -rf $(WWW_LIB)

clean::
	/bin/rm -rf $(DUNETGT)

.PHONY:: byte native d.byte clean web
