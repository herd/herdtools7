CAT2HTML7=$(if $(shell which cat2html7), cat2html7, ../_build/default/tools/cat2html.exe)
AARCH64_BOOKS= aarch64 aarch64-ifetch aarch64-mixed aarch64-MTE aarch64-MTE-mixed aarch64-VMSA aarch64-ETS2 aarch64-faults
BOOKS= $(AARCH64_BOOKS) bpf x86 linux
WWW_CATALOGUE=www/catalogue
HERD_CATALOGUE=../catalogue
PROG=jerd.js
WWW_PROG=www/$(PROG)
WWW_LIB=www/weblib
JSON_SHELVES=$(foreach book,$(BOOKS),$(WWW_CATALOGUE)/$(book)/shelf.json)
ARM_ARM_RELEASES=$(wildcard ../herd/libdir/aarch64/*)
ENV_PAIRS := $(foreach d,$(ARM_ARM_RELEASES),-env $(notdir $(d))=$(d))
CATINCLUDES=../herd/libdir linux $(foreach b,$(BOOKS),$(HERD_CATALOGUE)/$(b)/cats)
CATINCLUDESDEP=$(foreach d,$(CATINCLUDES) $(ARM_ARM_RELEASES),$(wildcard $(d)/*.*))
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
	$(if $(filter $1,$(AARCH64_BOOKS)), \
		$(foreach d,$(ARM_ARM_RELEASES), \
			mkdir -p $(WWW_CATALOGUE)/$1/cats/$(notdir $(d)) && \
			cp $(d)/aarch64.cat $(WWW_CATALOGUE)/$1/cats/$(notdir $(d))/aarch64.cat; \
		) \
	,)

.PHONY:: $(WWW_CATALOGUE)/$1/
endef
tst:
	ocaml generate_names.ml $(CATINCLUDES)
weblib:
	@echo '** INSTALL ' $(WWW_LIB) && mkdir -p $(WWW_LIB)
	@ocaml generate_names.ml -norec $(ENV_PAIRS) $(CATINCLUDES) | \
		while IFS=$$'\t' read -r rel src; do \
			mkdir -p "$(WWW_LIB)/$$(dirname "$$rel")" && \
			cp "$$src" "$(WWW_LIB)/$$rel" && \
			$(CAT2HTML7) "$(WWW_LIB)/$$rel" ; \
		done
	@find $(WWW_LIB) -type d -exec cp ./cat.css {}/cat.css \;

web: $(JSON_SHELVES) $(PROG) weblib
	@cp -v $(PROG) www/$(PROG)

$(PROG): catIncludes.ml force
	make -C .. Version.ml
	( cd .. && dune build --release @jerd )
	cp $(DUNETGT)/jerd.bc.js $(PROG) && chmod u+w $(PROG)

catIncludes.ml: generate_includes.ml $(CATINCLUDESDEP)
	ocaml ./generate_includes.ml -norec $(ENV_PAIRS) $(CATINCLUDES) > $@

$(foreach book,$(BOOKS),$(eval $(call make-json-shelf,$(book))))
$(foreach book,$(BOOKS),$(eval $(call make-book,$(book))))

clean::
	/bin/rm -f *~
	/bin/rm -f $(PROG) $(WWW_PROG) catIncludes.ml
	/bin/rm -f $(JSON_SHELVES)
	/bin/rm -rf $(WWW_LIB)

clean::
	/bin/rm -rf $(DUNETGT)

.PHONY:: force clean web
