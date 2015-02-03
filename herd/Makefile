PREFIX=$$HOME
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib/herd
J=-j 2
OCB=ocamlbuild $(OCBOPT) -cflags -w,A-4-9-29-45 -classic-display $(J)
EXTRAPROGS=
PROGS=herd.byte
MDL=x86tso.cat herd.cat herdcat.cat ppc.cat sc.cat LL.cat arm.cat power.cat \
 atom.cat atom-arm.cat pretty.cat prettycat.cat fulleieio.cat armllh.cat lessrelaxed.cat \
 uniproc.cat uniproccat.cat minimal.cat minimalcat.cat qualcomm.cat \
 cpp11.cat simplec11.cat opencl.cat sccat.cat ptx.cat simple-arm.cat \
 uniproc-normw-cat.cat mips.cat mips-tso.cat


CFG=apoil.cfg enslip.cfg web.cfg doc.cfg cpp11.cfg
DOT=test7.dot doc.dot

all: native

install:
	mkdir -p $(LIBDIR)
	cp $(MDL) $(DOT) $(CFG) $(LIBDIR)
#backward compatibility
	@for m in $(MDL:.cat=);\
	do ln -sf $(LIBDIR)/$$m.cat $(LIBDIR)/$$m.mdl; done
	@mkdir -p $(BINDIR)
	@for prog in $(PROGS:.byte=) ; \
	do cp $$prog $(BINDIR); done

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
	@@echo "let libdir = \"$(LIBDIR)\"" >> version.ml

baseline:
	@echo "Regenerating baseline..."
	@./herd -model power.cat testsuite/PPCTests/*  > testsuite/PPC_results_baseline.txt
	@./herd -model arm.cat testsuite/ARMTests/* > testsuite/ARM_results_baseline.txt
	@./herd -model cpp11.cat testsuite/CTests/* > testsuite/CPP11_results_baseline.txt
	@./herd -model ptx.cat testsuite/GPU_PTXTests/* > testsuite/PTX_results_baseline.txt
	@./herd -model opencl.cat testsuite/OpenCLTests/* > testsuite/OpenCL_results_baseline.txt

test:
	@echo "Running tests..."
	@./herd -model power.cat testsuite/PPCTests/*  > testsuite/PPC_results_new.txt
	@./herd -model arm.cat testsuite/ARMTests/* > testsuite/ARM_results_new.txt
	@./herd -model cpp11.cat testsuite/CTests/* > testsuite/CPP11_results_new.txt
	@./herd -model ptx.cat testsuite/GPU_PTXTests/* > testsuite/PTX_results_new.txt
	@./herd -model opencl.cat testsuite/OpenCLTests/* > testsuite/OpenCL_results_new.txt
	@test -s ../tools/mcompare || { echo "../tools/mcompare doesn't exist. Run the \`make -C ../tools\` command."; exit 1; }
	@echo "Comparing test results against baseline..."
	@../tools/mcompare \
          testsuite/PPC_results_new.txt \
          testsuite/PPC_results_baseline.txt \
          -neg testsuite/N1 -pos testsuite/P1 >/dev/null
	@../tools/mcompare \
          testsuite/ARM_results_new.txt \
          testsuite/ARM_results_baseline.txt \
          -neg testsuite/N2 -pos testsuite/P2 >/dev/null
	@../tools/mcompare \
          testsuite/CPP11_results_new.txt \
          testsuite/CPP11_results_baseline.txt \
          -neg testsuite/N3 -pos testsuite/P3 >/dev/null
	@../tools/mcompare \
          testsuite/PTX_results_new.txt \
          testsuite/PTX_results_baseline.txt \
          -neg testsuite/N4 -pos testsuite/P4 >/dev/null
	@../tools/mcompare \
          testsuite/OpenCL_results_new.txt \
          testsuite/OpenCL_results_baseline.txt \
          -neg testsuite/N5 -pos testsuite/P5 >/dev/null
	@echo "There are differences in the following PPC tests:" && \
	  cat testsuite/P1 && cat testsuite/N1
	@echo "There are differences in the following ARM tests:" && \
	  cat testsuite/P2 && cat testsuite/N2 
	@echo "There are differences in the following CPP11 tests:" && \
	  cat testsuite/P3 && cat testsuite/N3 
	@echo "There are differences in the following PTX tests:" && \
	  cat testsuite/P4 && cat testsuite/N4
	@echo "There are differences in the following OpenCL tests:" && \
	  cat testsuite/P5 && cat testsuite/N5
	@rm testsuite/P1 testsuite/N1 
	@rm testsuite/P2 testsuite/N2
	@rm testsuite/P3 testsuite/N3
	@rm testsuite/P4 testsuite/N4
	@rm testsuite/P5 testsuite/N5

clean::
	/bin/rm -f version.ml
	/bin/rm -f testsuite/PPC_results_new.txt
	/bin/rm -f testsuite/ARM_results_new.txt
	/bin/rm -f testsuite/CPP11_results_new.txt
	/bin/rm -f testsuite/PTX_results_new.txt
	/bin/rm -f testsuite/OpenCL_results_new.txt

