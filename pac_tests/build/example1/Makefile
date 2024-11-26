AUXFLAGS=0x0
SRCDIR=$(PWD)/..
-include $(SRCDIR)/config.mak
libcflat = $(SRCDIR)/lib/libcflat.a
libeabi = $(SRCDIR)/lib/arm/libeabi.a
LIBFDT_archive = $(SRCDIR)/lib/libfdt/libfdt.a
cstart.o = $(SRCDIR)/arm/cstart64.o
FLATLIBS = $(libcflat) $(LIBFDT_archive) $(libeabi)
optional-ccopt = $(shell if $(CC) -Werror $(1) -S -o /dev/null -xc /dev/null > /dev/null 2>&1; then echo "$(1)"; fi)
GCCOPTS=-Wall -std=gnu99 -std=gnu99 -ffreestanding -I $(SRCDIR)/lib -I $(SRCDIR)/libfdt -Wall -Werror  -fomit-frame-pointer -Wno-frame-address   -fno-pic  -no-pie -Wmissing-parameter-type -Wold-style-declaration -Woverride-init -O2 $(call optional-ccopt, -mno-outline-atomics) -march=armv8.1-a
LINKOPTS=
SRC=\
 example1.c\

UTILS=litmus_rand.o utils.o kvm_timeofday.o
EXE=$(SRC:.c=.flat)
T=$(SRC:.c=.t)

all: $(EXE) $(T)

clean:
	/bin/rm -f *.o *.s *.t *.elf *.flat *~ $(H)

cleansource:
	/bin/rm -f *.o *.c *.h *.s *~

%.s: %.c
	$(CC) $(GCCOPTS) -S $<
%.o: %.c
%.o: %.s
	$(CC) $(GCCOPTS) -c -o $@ $<
%.t: %.s
	awk -f show.awk $< > $@

%.elf: LDFLAGS = -nostdlib -pie -n
%.elf: %.o $(UTILS) $(FLATLIBS) $(SRCDIR)/arm/flat.lds $(cstart.o)
	$(CC) $(CFLAGS) -c -o $(@:.elf=.aux.o) $(SRCDIR)/lib/auxinfo.c \
		-DPROGNAME=\"$(@:.elf=.flat)\" -DAUXFLAGS=$(AUXFLAGS)
	$(LD) $(LDFLAGS) -o $@ -T $(SRCDIR)/arm/flat.lds \
		$(filter %.o, $^) $(FLATLIBS) $(@:.elf=.aux.o)
	$(RM) $(@:.elf=.aux.o)
	@chmod a-x $@

%.flat: %.elf
	$(call arch_elf_check, $^)
	$(OBJCOPY) -O binary $^ $@
	@chmod a-x $@
