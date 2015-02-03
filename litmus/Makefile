PREFIX=$$HOME
BINDIR=$(PREFIX)/bin
LIBDIR=$(PREFIX)/lib/litmus
CFG=conti.cfg saumur.cfg saumur.close.cfg doko.cfg squale.cfg hpcx.cfg \
 vargas.cfg vargas.cache.cfg vargas.close.cfg vargas16.cfg \
 abducens.cfg  abducens.big.cfg abducens.cache.cfg abducens.close.cfg \
 chianti.cfg portable.cfg beaune.cfg \
 power7.cfg power7.32.cfg power.cfg g5.cfg\
 ARM.cfg ARM_ADB.cfg cortex9.cfg cortex9-O0.cfg\
 dragon.cfg dragon-arm.cfg dragon-thumb.cfg\
 trimslice.cfg\
 tst.cfg auto.cfg power7.smt.cfg power6.smt.cfg \
 mp.cfg ipad.cfg asus.cfg asus-debian.cfg asus-debian-thumb.cfg cortexA15.cfg \
 mike.cfg youpla.cfg hopla.cfg corei7.cfg x86.cfg nexus10.cfg nexus7.cfg \
 odroid-xu.cfg odroid-xu-ubuntu.cfg \
 cubietruck.cfg R4000.cfg

INSERT_XXX=mbar.c barrier.c
INSERT_X86=$(INSERT_XXX) timebase.c cache.c
INSERT_PPC=$(INSERT_XXX) timebase32.c timebase64.c barrier_lab.c cache.c
INSERT_ARM=$(INSERT_XXX) cache.c
INSERT_MIPS=$(INSERT_XXX)

PLATFORM_XXX=platform_io.h platform_io.c
PLATFORM_LINUX=$(PLATFORM_XXX)

OCBOPT=
OCB=ocamlbuild $(OCBOPT) -cflags -w,A-4-9-27-29-45 -classic-display
all: native

install:
	mkdir -p $(BINDIR)
	cp litmus $(BINDIR)
	mkdir -p $(LIBDIR)
	cp _utils.c _utils.h $(LIBDIR)
	cp _litmus_rand.c _litmus_rand.h $(LIBDIR)
	cp _litmus_io.c _litmus_io.h $(LIBDIR)
	cp _presi.c _presi.h $(LIBDIR)
	cp _outs.c _outs.h $(LIBDIR)
	cp _aix_affinity.c _linux_affinity.c _affinity.h $(LIBDIR)
	cp _show.awk _showC.awk _toh.sh $(LIBDIR)
	cp _mix.h _hash.c _instance.c _main.c $(LIBDIR)
	cp $(CFG) $(LIBDIR)
	mkdir -p  $(LIBDIR)/_ppc && ( cd _ppc && cp $(INSERT_PPC) $(LIBDIR)/_ppc )
	mkdir -p  $(LIBDIR)/_x86 && ( cd _x86 && cp $(INSERT_X86) $(LIBDIR)/_x86 )
	mkdir -p  $(LIBDIR)/_arm  && ( cd _arm && cp $(INSERT_ARM) $(LIBDIR)/_arm )
	/bin/rm -rf  $(LIBDIR)/_mips && mkdir -p  $(LIBDIR)/_mips  && ( cd _mips && cp $(INSERT_MIPS) $(LIBDIR)/_mips )
	/bin/rm -rf $(LIBDIR)/_linux && mkdir -p $(LIBDIR)/_linux && ( cd _linux && cp $(PLATFORM_LINUX) $(LIBDIR)/_linux )

luc: all install

#To build several targets with ocamlbuild
PROGS=litmus.byte

byte native d.byte: config ./pp2ml.byte
	sh lib/handle402.sh
	$(OCB) $(PROGS:.byte=.$@)
	@for prog in $(PROGS:.byte=.$@) ; \
	do ln -sf $$prog `basename $$prog .$@` ; done

clean::
	/bin/rm -f *~ litmus mcycles *.native *.byte *.d.byte

clean::
	$(OCB) -clean

SVNVERSION=sh -c "git rev-parse HEAD" 2>/dev/null || echo exported

.PHONY: config

config:
	@echo "(* GENERATED, do not edit *)" > config.ml
	@echo 'let version = "'`head -1 lib/VERSION.txt`'"' >> config.ml
	@echo "let libdir = \"$(LIBDIR)\"" >> config.ml
	@export LC_ALL=C && echo "let rev = \"`$(SVNVERSION)`\"" >> config.ml

./pp2ml.byte: ./pp2ml.mll
	$(OCB) $@

clean::
	/bin/rm -f ./pp2ml.byte

clean::
	/bin/rm -f config.ml
