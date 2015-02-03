PREFIX=/usr/local
DIRS=litmus gen

default: all

all install clean:
	for d in $(DIRS) ; \
	do $(MAKE) $(MFLAGS) PREFIX=$(PREFIX) -C ../$$d $@ ; done

