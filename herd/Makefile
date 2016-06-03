PREFIX=$$HOME
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib/herd7
J=-j 2
OCB=ocamlbuild $(OCBOPT) -cflags -bin-annot,-w,A-4-9-29-45 -classic-display $(J)
EXTRAPROGS=
PROGS=herd.byte
MDL=x86tso.cat herd.cat herdcat.cat ppc.cat sc.cat LL.cat arm.cat \
 atom.cat atom-arm.cat pretty.cat prettycat.cat fulleieio.cat armllh.cat lessrelaxed.cat \
 naked.cat uni.cat uniproc.cat uniproccat.cat minimal.cat minimalcat.cat mini.cat qualcomm.cat \
 simple-c11.cat sccat.cat simple-arm.cat \
 uniproc-normw-cat.cat mips.cat mips-tso.cat \
 fences.cat ppcfences.cat armfences.cat x86fences.cat mipsfences.cat aarch64fences.cat\
 cross.cat cos.cat cos-opt.cat cosllh.cat stdlib.cat ppo.cat filters.cat compat.cat \
 ppc-checks.cat \
 aarch64.cat doc64.cat




CFG=apoil.cfg enslip.cfg web.cfg doc.cfg cpp11.cfg
DOT=test7.dot doc.dot

all: native

install:
	mkdir -p $(LIBDIR)
	cp $(MDL) $(DOT) $(CFG) $(LIBDIR)
	cp ppc.cat $(LIBDIR)/power.cat
#backward compatibility
	@mkdir -p $(BINDIR)
	@for prog in $(PROGS:.byte=) ; \
	do cp $$prog $(BINDIR)/$${prog}7; done
	@for f in c11/*.cat c11/*.def ; do cp $$f $(LIBDIR); done

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

SVNVERSION=sh -c "svnversion" 2>/dev/null || echo exported

version:
	@echo "(* GENERATED, DO NOT EDIT *)" > version.ml
	@echo  'let version = "'`head -1 lib/VERSION.txt`'"' >> version.ml
	@export LC_ALL=C && echo  'let rev = "'`$(SVNVERSION)`'"' >> version.ml
	@@echo "let libdir = \"$(LIBDIR)\"" >> version.ml

clean::
	/bin/rm -f version.ml

