PREFIX=$$HOME
BINDIR=$(PREFIX)/bin
OCB=ocamlbuild $(OCBOPT) -cflags -w,A-4-9-29-41-45 -classic-display $(J)
EXTRAPROGS=atomize.byte mexpand.byte nexts.byte atoms.byte
PROGS=diy.byte diyone.byte readRelax.byte dont.byte diycross.byte classify.byte $(EXTRAPROGS)



all: native

install:
	@mkdir -p $(BINDIR)
	@for prog in $(PROGS:.byte=) ; \
	do cp $$prog $(BINDIR); done


luc: native install


byte native d.byte: version
	sh lib/handle402.sh
	$(OCB) $(PROGS:.byte=.$@)
	@for prog in $(PROGS:.byte=.$@) ; \
	do ln -sf $$prog `basename $$prog .$@` ; done

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
	@echo  'let rev = "'`$(SVNVERSION)`'"' >> version.ml
clean::
	/bin/rm -f version.ml

