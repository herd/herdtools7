##Top level makefile for local installation of source tree
SRC=herd gen litmus tools
PREFIX=$$HOME
OCBOPT=
default: all

luc all install clean:
	for d in $(SRC) ; \
	do $(MAKE) $(MFLAGS) PREFIX=$(PREFIX) OCBOPT=$(OCBOPT) -C $$d $@ ; done
