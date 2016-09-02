PREFIX=$$HOME
BINDIR=$(PREFIX)/bin
J=-j 2
OCB=ocamlbuild $(OCBOPT) -cflags -bin-annot,-w,A-4-9-29-45 -classic-display $(J)
EXTRAPROGS=
PROGS=jingle.byte

all: native

install:
	@mkdir -p $(BINDIR)
	@for prog in $(PROGS:.byte=) ; \
	do cp $$prog $(BINDIR)/$${prog}7; done

luc: native install


byte native d.byte: version
	sh lib/handle402.sh
	$(OCB) $(PROGS:.byte=.$@)
	@for prog in $(PROGS:.byte=.$@) ; \
	do ln -sf $$prog `basename $$prog .$@` ; done

doc:
	$(OCB) documentation.docdir/index.html

clean::
	-$(OCB) -clean

clean::
	/bin/rm -f *~
	@for prog in $(PROGS:.byte=) ; \
	do /bin/rm -f $$prog $$prog.byte $$prog.native ; done

SVNVERSION=sh -c "LC_ALL=C git rev-parse HEAD" 2>/dev/null || echo exported

version:
	@echo "(* GENERATED, DO NOT EDIT *)" > version.ml
	@echo  'let version = "'`head -1 lib/VERSION.txt`'"' >> version.ml
	@export LC_ALL=C && echo  'let rev = "'`$(SVNVERSION)`'"' >> version.ml


clean::
	rm -f version.ml
