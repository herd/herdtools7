.PHONY: check-deps

OS := $(shell uname)
PREFIX=$$HOME
D=dune

#Limit parallelism of some expensive operations
ifeq ($(OS),Darwin)
	J=$(shell sysctl -n hw.logicalcpu)
else
	J=$(shell nproc)
endif


REGRESSION_TEST_MODE = test
# REGRESSION_TEST_MODE = promote
# REGRESSION_TEST_MODE = show

DIYCROSS                      = _build/install/default/bin/diycross7
HERD                          = _build/install/default/bin/herd7
HERD_REGRESSION_TEST          = _build/default/internal/herd_regression_test.exe
HERD_DIYCROSS_REGRESSION_TEST = _build/default/internal/herd_diycross_regression_test.exe
HERD_CATALOGUE_REGRESSION_TEST = _build/default/internal/herd_catalogue_regression_test.exe


all: build

.PHONY: Version.ml
Version.ml:
	sh ./version-gen.sh $(PREFIX)

build: Version.ml | check-deps
	dune build -j $(J) --profile release

install:
	sh ./dune-install.sh $(PREFIX)

uninstall:
	sh ./dune-uninstall.sh $(PREFIX)

clean: dune-clean clean-asl-pseudocode
	rm -f Version.ml

dune-clean:
	dune clean

versions: Version.ml
	@ dune build -j $(J) --workspace dune-workspace.versions @default


# Dependencies.

check-deps::
	$(if $(shell which ocaml),,$(error "Could not find ocaml in PATH"))
	$(if $(shell which menhir),,$(error "Could not find menhir in PATH; it can be installed with `opam install menhir`."))

check-deps::
	$(if $(shell which dune),,$(error "Could not find dune in PATH; it can be installed with `opam install dune`."))

# Tests.
TIMEOUT=16.0

test:: | build

test:: dune-test
	@ echo "OCaml unit tests: OK"

dune-test:
	@ echo
	dune runtest --profile=release

test:: test.aarch64
test.aarch64:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64 \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 instructions tests: OK"

test:: test.riscv
test.riscv:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/RISCV \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 RISCV instructions tests: OK"

test:: test.x86_64
test.x86_64:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/X86_64 \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 X86_64 instructions tests: OK"

test:: test.mixed
test.mixed:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.mixed \
		-conf ./herd/tests/instructions/AArch64.mixed/mixed.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 mixed instructions tests: OK"

test:: test.mips
test.mips:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/MIPS \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 RISCV instructions tests: OK"

test:: test.neon
test.neon::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.neon \
		-variant neon \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 NEON instructions tests: OK"

test:: test.mte
test.mte::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.MTE \
		-conf ./herd/tests/instructions/AArch64.MTE/mte.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 MTE instructions tests: OK"

test:: test.self
test.self:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.self \
		-conf ./herd/tests/instructions/AArch64.self/self.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 variant -self instructions tests: OK"

test:: test.kvm
test.kvm:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.kvm \
		-conf ./herd/tests/instructions/AArch64.kvm/kvm.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 KVM instructions tests: OK"

test::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/C \
		-conf ./herd/tests/instructions/C/c.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 C instructions tests: OK"

test:: test-ppc
test-ppc:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/PPC \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 PPC instructions tests: OK"

test:: test-asl
test-asl:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/ASL \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 ASL instructions tests: OK"

test:: test-pseudo-asl
test-pseudo-asl:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/ASL-pseudo-arch \
		-conf ./herd/tests/instructions/ASL-pseudo-arch/pseudo-conf.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 ASL instructions tests on pseudo-architecture: OK"

test-aarch64-asl: asl-pseudocode
	@echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.ASL \
		-conf ./herd/tests/instructions/AArch64.ASL/asl.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64+ASL instructions tests: OK"

test:: arm-test

arm-test::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/ARM \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 ARM instructions tests: OK"

aarch32-test::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch32 \
		-conf ./herd/tests/instructions/AArch32/aarch32.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 ARM instructions tests: OK"

test::aarch32-test

test::

diy-test:: diy-test-aarch64
diy-test-aarch64:
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-diycross-path $(DIYCROSS) \
		-libdir-path ./herd/libdir \
		-expected-dir ./herd/tests/diycross/AArch64 \
		-diycross-arg -arch \
		-diycross-arg AArch64 \
		-diycross-arg 'Pod**,Fenced**' \
		-diycross-arg 'Rfe,Fre,Coe' \
		-diycross-arg 'Pod**,Fenced**,DpAddrdR,DpAddrdW,DpDatadW,CtrldR,CtrldW' \
		-diycross-arg 'Rfe,Fre,Coe' \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 diycross7 tests: OK"

test:: cata-test more-test

cata-test:: cata-aarch64-test
cata-aarch64-test:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-herd-timeout $(TIMEOUT) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64/tests/kinds.txt \
		-shelf-path catalogue/aarch64/shelf.py \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64 tests: OK"

more-test:: aarch64-test-mixed
aarch64-test-mixed:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-herd-timeout $(TIMEOUT) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64/tests/kinds.txt \
		-shelf-path catalogue/aarch64/shelf.py \
		-variant mixed \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64 tests (mixed mode): OK"


cata-test:: mixed-test
mixed-test:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-mixed/tests/kinds.txt \
		-shelf-path catalogue/aarch64-mixed/shelf.py \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64-mixed tests: OK"

cata-test:: pick-test
pick-test:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-pick/tests/desired-kinds.txt \
		-shelf-path catalogue/aarch64-pick/shelf.py \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64-pick tests: OK"

cata-test:: faults-test
faults-test:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-faults/tests/kinds.txt \
		-shelf-path catalogue/aarch64-faults/shelf.py \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64-faults tests: OK"

more-test:: pick-test-mixed
pick-test-mixed:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-herd-timeout $(TIMEOUT) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-pick/tests/desired-kinds.txt \
		-shelf-path catalogue/aarch64-pick/shelf.py \
		-variant mixed \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64-pick tests (mixed mode): OK"

more-test:: mte-test
mte-test:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-MTE/tests/kinds.txt \
		-shelf-path catalogue/aarch64-MTE/shelf.py \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64-MTE tests: OK"

vmsa-test:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-herd-timeout $(TIMEOUT) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-VMSA/tests/VMSA-kinds.txt \
		-shelf-path catalogue/aarch64-VMSA/shelf.py \
		$(REGRESSION_TEST_MODE)
		@ echo "herd7 catalogue aarch64-VMSA tests: OK"

ets2-test:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-herd-timeout $(TIMEOUT) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-ETS2/tests/VMSA-ETS2-kinds.txt \
		-shelf-path catalogue/aarch64-ETS2/shelf.py \
		$(REGRESSION_TEST_MODE)
		@ echo "herd7 catalogue aarch64-ETS2 tests: OK"

test:: diy-test

LDS:="Amo.Cas,Amo.LdAdd,Amo.LdClr,Amo.LdEor,Amo.LdSet"
LDSPLUS:="LxSx",$(LDS)

diy-test:: diy-test-mixed
diy-test-mixed::
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-diycross-path $(DIYCROSS) \
		-libdir-path ./herd/libdir \
		-expected-dir ./herd/tests/diycross/AArch64.mixed \
		-conf ./herd/tests/diycross/AArch64.mixed/mixed.cfg \
		-diycross-arg -ua \
		-diycross-arg 0 \
		-diycross-arg -obs \
		-diycross-arg oo \
		-diycross-arg -arch \
		-diycross-arg AArch64 \
		-diycross-arg -variant \
		-diycross-arg mixed \
		-diycross-arg -hexa \
		-diycross-arg Hat \
		-diycross-arg h0 \
		-diycross-arg $(LDSPLUS) \
		-diycross-arg h0 \
		-diycross-arg Rfi \
		-diycross-arg w0 \
		-diycross-arg Amo.StAdd \
		-diycross-arg w0 \
		-diycross-arg Rfi \
		-diycross-arg h2 \
		-diycross-arg $(LDS) \
		-diycross-arg h2 \
		-diycross-arg PodWR \
		-diycross-arg Hat \
		-diycross-arg w0 \
		-diycross-arg Amo.LdSet \
		-diycross-arg w0 \
		-diycross-arg PodWR \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64.mixed diycross7 tests: OK"

diy-test-mixed::
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-diycross-path $(DIYCROSS) \
		-libdir-path ./herd/libdir \
		-expected-dir ./herd/tests/diycross/AArch64.mixed.strict \
		-conf ./herd/tests/diycross/AArch64.mixed.strict/mixed.cfg \
		-diycross-arg -arch \
		-diycross-arg AArch64 \
		-diycross-arg -ua \
		-diycross-arg 0 \
		-diycross-arg -variant \
		-diycross-arg mixed,MixedStrictOverlap \
		-diycross-arg -hexa \
		-diycross-arg h0,h2,w0 \
		-diycross-arg Amo.CasAP,LxSxAP \
		-diycross-arg h0,h2,w0  \
		-diycross-arg PodWR \
		-diycross-arg w0,h0 \
		-diycross-arg Fre \
		-diycross-arg w0,h2 \
		-diycross-arg FencedWW \
		-diycross-arg w0,h0,h2 \
		-diycross-arg Rfe \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64.mixed.strict diycross7 tests: OK"

diy-test-mixed:: v32 v64

v32:
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-diycross-path $(DIYCROSS) \
		-libdir-path ./herd/libdir \
		-expected-dir ./herd/tests/diycross/AArch64.mixed.v32 \
		-conf ./herd/tests/diycross/AArch64.mixed.strict/mixed.cfg \
		-diycross-arg -arch \
		-diycross-arg AArch64 \
		-diycross-arg -variant \
		-diycross-arg mixed \
		-diycross-arg -hexa \
		-diycross-arg PodWW \
		-diycross-arg RfeLA \
		-diycross-arg h0,h2,w0 \
		-diycross-arg DpDatadW,DpAddrdR,DpAddrdW \
		-diycross-arg A,P,L \
		-diycross-arg h0,h2,w0 \
		-diycross-arg Coe,Fre \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64.mixed.v32 diycross7 tests: OK"

v64:
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-diycross-path $(DIYCROSS) \
		-libdir-path ./herd/libdir \
		-expected-dir ./herd/tests/diycross/AArch64.mixed.v64 \
		-conf ./herd/tests/diycross/AArch64.mixed.strict/mixed.cfg \
		-diycross-arg -arch \
		-diycross-arg AArch64 \
		-diycross-arg -variant \
		-diycross-arg mixed \
		-diycross-arg -hexa \
		-diycross-arg -type \
		-diycross-arg uint64_t \
		-diycross-arg PodWW \
		-diycross-arg RfeLA \
		-diycross-arg w0,w4,q0 \
		-diycross-arg DpDatadW,DpAddrdR,DpAddrdW \
		-diycross-arg A,P,L \
		-diycross-arg w0,w4,q0 \
		-diycross-arg Coe,Fre \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64.mixed.v64 diycross7 tests: OK"

diy-test:: diy-store-test
diy-store-test:
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-diycross-path $(DIYCROSS) \
		-libdir-path ./herd/libdir \
		-expected-dir ./herd/tests/diycross/AArch64.store \
		-diycross-arg -obs \
		-diycross-arg four \
		-diycross-arg -arch \
		-diycross-arg AArch64 \
		-diycross-arg 'Fenced**' \
		-diycross-arg 'Rfe,Fre,Coe' \
		-diycross-arg 'DpAddrdR,DpDatadW' \
		-diycross-arg 'Pos**' \
		-diycross-arg 'Store' \
		-diycross-arg 'Rfe,Fre,Coe' \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 diycross7.store tests: OK"

diy-test:: diy-test-mte
diy-test-mte::
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-diycross-path $(DIYCROSS) \
		-libdir-path ./herd/libdir \
		-expected-dir ./herd/tests/diycross/AArch64.MTE \
		-conf ./herd/tests/diycross/AArch64.MTE/MTE.cfg \
		-diycross-arg -arch \
		-diycross-arg AArch64 \
		-diycross-arg -variant \
		-diycross-arg memtag \
		-diycross-arg DMB.SYd*W \
		-diycross-arg T,P \
		-diycross-arg Rfe \
		-diycross-arg A \
		-diycross-arg Amo.LdAdd \
		-diycross-arg L \
		-diycross-arg PodW* \
		-diycross-arg T,P \
		-diycross-arg Coe,Rfe,Fre \
		-diycross-arg T,P \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64.MTE diycross7 tests: OK"

.PHONY: asl-pseudocode clean-asl-pseudocode
asl-pseudocode: herd/libdir/asl-pseudocode/shared_pseudocode.asl
herd/libdir/asl-pseudocode/shared_pseudocode.asl:
	@ $(MAKE) -C $(@D) a64 clean-tmp

clean-asl-pseudocode:
	@ $(MAKE) -C $(@D)/herd/libdir/asl-pseudocode clean
