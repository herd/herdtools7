PREFIX=$$HOME
BINDIR=$(PREFIX)/bin

OCB=ocamlbuild $(OCBOPT) -cflags -w,A-4-29-41-45 -classic-display $(J)

EXTRAPROGS=
SCRIPTS=mnstates
PROGS=mcycles.byte msort.byte mapply.byte msum.byte mdiff.byte mcompare.byte mnames.byte mfind.byte mcond.byte recond.byte splitdot.byte mprog.byte mfilter.byte mlog2cond.byte moutcomes.byte mobserved.byte mproj.byte splitcond.byte ksort.byte knames.byte mtopos.byte mmixer.byte


all: native

install:
	@mkdir -p $(BINDIR)
	@for prog in $(PROGS:.byte=) $(SCRIPTS) ; \
	do cp $$prog $(BINDIR); done
	ln -f $(BINDIR)/mdiff $(BINDIR)/minter

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

SVNVERSION=sh -c "LC_ALL=C svnversion" 2>/dev/null || echo exported

version:
	@echo "(* GENERATED, DO NOT EDIT *)" > version.ml
	@echo  'let version = "'`head -1 lib/VERSION.txt`'"' >> version.ml
	@export LC_ALL=C && echo  'let rev = "'`$(SVNVERSION)`'"' >> version.ml
	@@echo "let libdir = \"$(LIBDIR)\"" >> version.ml

clean::
	/bin/rm -f version.ml

