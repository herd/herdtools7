PREFIX=$$HOME
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib/herd7
J=-j 8
JERD_OPT=-use-ocamlfind
BOOKS=armed-cats demi demo tutorial ptx herding-cats v8-proposal c11popl15 java linux hsa
WWW_CATALOGUE=www/catalogue
HERD_CATALOGUE=../catalogue
OCB=ocamlbuild $(OCBOPT) $(JERD_OPT) -cflags -bin-annot,-w,A-4-9-29-45,-g -classic-display $(J)
PROG=jerd.js
BYTE=jerd.byte
WWW_PROG=www/$(PROG)
JSON_SPINES=$(foreach book,$(BOOKS),$(WWW_CATALOGUE)/$(book)/shelf.json)
CATINCLUDES=$(wildcard cat_includes/*.cat)

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

web: $(JSON_SPINES) $(PROG)
	@cp -v $(PROG) www/$(PROG)

#js_of_ocaml --pretty --no-inline --debug-info $(BYTE)
$(PROG): byte
	js_of_ocaml $(BYTE)

catIncludes.ml: $(CATINCLUDES)
	./generate_includes.py > $@

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

clean::

.PHONY:: byte native d.byte clean web
