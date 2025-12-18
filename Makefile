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

DUNE_PROFILE = release

DIY                           = _build/install/default/bin/diy7
DIYCROSS                      = _build/install/default/bin/diycross7
DIYMICROENUM                  = _build/install/default/bin/diymicroenum7
HERD                          = _build/install/default/bin/herd7
LITMUS                        = _build/install/default/bin/litmus7
LITMUS_LIB_DIR                = $(PWD)/litmus/libdir
DIY_REGRESSION_TEST           = _build/default/internal/diy_regression_test.exe
HERD_REGRESSION_TEST          = _build/default/internal/herd_regression_test.exe
HERD_DIYCROSS_REGRESSION_TEST = _build/default/internal/herd_diycross_regression_test.exe
HERD_CATALOGUE_REGRESSION_TEST = _build/default/internal/herd_catalogue_regression_test.exe
BENTO                         = _build/default/tools/bento.exe
ASLREF                        = _build/default/asllib/aslref.exe
CHECK_OBS                     = _build/default/internal/check_obs.exe
all: build

.PHONY: Version.ml
Version.ml:
	sh ./version-gen.sh $(PREFIX)

just-build: Version.ml
	dune build -j $(J) --profile $(DUNE_PROFILE)

build-release: Version.ml
	dune build -j $(J) -p herdtools7 @install

build: check-deps | just-build

install:
	sh ./dune-install.sh $(PREFIX)

uninstall:
	sh ./dune-uninstall.sh $(PREFIX)

clean: dune-clean clean-asl-pseudocode clean-asldoc
	rm -f Version.ml

dune-clean:
	dune clean

versions: Version.ml
	@ dune build -j $(J) --workspace dune-workspace.versions


# Dependencies.

check-deps::
	$(if $(shell which ocaml),,$(error "Could not find ocaml in PATH"))
	$(if $(shell which menhir),,$(error "Could not find menhir in PATH; it can be installed with `opam install menhir`."))

check-deps::
	$(if $(shell which dune),,$(error "Could not find dune in PATH; it can be installed with `opam install dune`."))

# Tests.
TIMEOUT=16.0

test-all:: test
test:: | build

test:: dune-test
	@ echo "OCaml unit tests: OK"

dune-test:
	@ echo
	dune runtest --profile=$(DUNE_PROFILE)

.PHONY: dune-no-missing-file-in-runt
test:: dune-no-missing-file-in-runt
dune-no-missing-file-in-runt:
	asllib/tests/check-no-missing-file-in-run.sh ./

test:: test.aarch64
test-local:: test.aarch64
test.aarch64:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64 \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 instructions tests: OK"

test:: test.aarch64.asl
test-all-asl:: test.aarch64.asl
test.aarch64.asl: asl-pseudocode
	@ echo
	$(HERD_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64 \
		-conf ./herd/tests/instructions/AArch64/asl.cfg \
		-checkstates \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 instructions tests (ASL): OK"

test-all-asl:: test.aarch64.asl.with.vmsa
test.aarch64.asl.with.vmsa: asl-pseudocode
	@ echo
	$(HERD_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64 \
		-conf ./herd/tests/instructions/AArch64/asl-with-vmsa.cfg \
		-checkstates \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 instructions tests (ASL with VMSA): OK"

test:: test.riscv
test-local:: test.riscv
test.riscv:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/RISCV \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 RISCV instructions tests: OK"

test:: test.x86_64
test-local:: test.x86_64
test.x86_64:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/X86_64 \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 X86_64 instructions tests: OK"

test:: test.mixed
test-local:: test.mixed
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
test-local:: test.mips
test.mips:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/MIPS \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 MIPS instructions tests: OK"

test:: test.neon
test-local:: test.neon
test.neon::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.neon \
		-variant neon \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 NEON instructions tests: OK"

test:: test.sve
test-local:: test.sve
test.sve::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.sve \
		-variant sve \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 SVE instructions tests: OK"

test:: test.asl.sve
test-local:: test.asl.sve
test-all-asl:: test.asl.sve
test.asl.sve: asl-pseudocode
	@ echo
	$(HERD_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.sve \
		-variant sve \
		-conf  ./herd/tests/instructions/AArch64.sve/asl.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 ASL SVE instructions tests: OK"

test:: test.sme
test-local:: test.sme
test.sme::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.sme \
		-variant sme \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 SME instructions tests: OK"

test:: test.mte
test-local:: test.mte
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
test-local:: test.self
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
test-local:: test.kvm
test.kvm:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.kvm \
		-conf ./herd/tests/instructions/AArch64.kvm/kvm.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 KVM instructions tests: OK"

test:: test-asl-vmsa
test-asl-vmsa:: test.kvm.asl
test-all-asl:: test.kvm.asl
test.kvm.asl: asl-pseudocode
	@ echo
	$(HERD_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.kvm \
		-conf ./herd/tests/instructions/AArch64.kvm/asl-vmsa.cfg \
		-checkstates \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 KVM (ASL) instructions tests: OK"

test:: test-c
test-local:: test-c
test-c:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/C \
		-conf ./herd/tests/instructions/C/c.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 C instructions tests: OK"

test:: test-ppc
test-local:: test-ppc
test-ppc:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/PPC \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 PPC instructions tests: OK"

test:: test-asl
test-local:: test-asl
test-all-asl:: test-asl
test-asl: asl-pseudocode
	@ echo
	$(HERD_REGRESSION_TEST) \
		-nohash \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/ASL \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 ASL instructions tests: OK"

test:: test-pseudo-asl
test-local:: test-pseudo-asl
test-all-asl:: test-pseudo-asl
test-pseudo-asl:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-nohash \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/ASL-pseudo-arch \
		-conf ./herd/tests/instructions/ASL-pseudo-arch/pseudo-conf.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 ASL instructions tests on pseudo-architecture: OK"

test:: test-aarch64-asl
test-all-asl:: test-aarch64-asl
test-aarch64-asl: asl-pseudocode
	@echo
	$(HERD_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.ASL \
		-conf ./herd/tests/instructions/AArch64.ASL/asl.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64+ASL instructions tests: OK"

test-all-asl:: test-aarch64-asl-with-vmsa
test-aarch64-asl-with-vmsa: asl-pseudocode
	@echo
	$(HERD_REGRESSION_TEST) \
		-j $(J) -checkstates  \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.ASL \
		-conf ./herd/tests/instructions/AArch64.ASL/asl-with-vmsa.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64+ASL (with VMSA) instructions tests: OK"

test:: test-aarch64-noasl
test-local:: test-aarch64-noasl
test-aarch64-noasl:
	@echo
	$(HERD_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.ASL \
		-conf ./herd/tests/instructions/AArch64.ASL/noasl.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64+NOASL instructions tests: OK"

test:: test-aarch64-noasl-mixed
test-local:: test-aarch64-noasl-mixed
test-aarch64-noasl-mixed:
	@echo
	$(HERD_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.ASL \
		-conf ./herd/tests/instructions/AArch64.ASL/noasl-mixed.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64+NOASL+MIXED instructions tests: OK"

test:: arm-test
test-local:: arm-test

arm-test::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/ARM \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 ARM instructions tests: OK"

test::aarch32-test
test-local::aarch32-test
aarch32-test::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch32 \
		-conf ./herd/tests/instructions/AArch32/aarch32.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch32 instructions tests: OK"

test-bnfc:
	@ echo
	dune runtest asllib/menhir2bnfc
	@ echo "BNFC tests: OK"

test:: test.pac
test-local:: test.pac
test.pac::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.PAC \
		-conf ./herd/tests/instructions/AArch64.PAC/pac.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 PAC instructions tests: OK"


### CATALOGUE testing, catalogue must be here
CATATEST := $(shell if test -d catalogue; then echo cata-test; fi)

test:: $(CATATEST)
test-local:: $(CATATEST)

test-all:: cata-test cata-test-asl
test-all-asl:: cata-test-asl

cata-test:: cata-bpf-test
cata-bpf-test:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-herd-timeout $(TIMEOUT) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/bpf/tests/kinds.txt \
		-shelf-path catalogue/bpf/shelf.py \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue bpf tests: OK"

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

cata-test-asl:: cata-aarch64-test-asl
cata-aarch64-test-asl: asl-pseudocode
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-variant asl \
		-variant strict \
		-herd-timeout $(TIMEOUT) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64/tests/kinds.txt \
		-shelf-path catalogue/aarch64/shelf.py \
		-conf-path catalogue/aarch64/cfgs/asl.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64 tests (ASL): OK"

cata-test-asl:: cata-aarch64-cas-test-asl
cata-aarch64-cas-test-asl: asl-pseudocode
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-variant asl \
		-variant strict \
		-herd-timeout $(TIMEOUT) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-cas/tests/kinds.txt \
		-shelf-path catalogue/aarch64-cas/shelf.py \
		-conf-path catalogue/aarch64-cas/cfgs/asl.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64-cas tests (ASL): OK"

cata-test:: aarch64-test-mixed
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

cata-test-asl:: pick-test-asl
pick-test-asl:  asl-pseudocode
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-variant asl \
		-variant strict \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-pick/tests/desired-kinds.txt \
		-shelf-path catalogue/aarch64-pick/shelf.py \
		-conf-path catalogue/aarch64-pick/cfgs/asl.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64-pick tests (ASL): OK"

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

cata-test-asl:: asl-faults-test
asl-faults-test: asl-pseudocode
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-faults/tests/kinds.txt \
		-shelf-path catalogue/aarch64-faults/shelf.py \
		-conf-path  catalogue/aarch64-faults/cfgs/asl.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64-faults tests (ASL): OK"

cata-test:: pick-test-mixed
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

cata-test:: mte-test
mte-test:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-herd-timeout $(TIMEOUT) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-MTE/tests/kinds.txt \
		-shelf-path catalogue/aarch64-MTE/shelf.py \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64-MTE tests: OK"

cata-test:: ifetch-test
ifetch-test:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-herd-timeout $(TIMEOUT) \
		-j $(J) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-ifetch/kinds.txt \
		-shelf-path catalogue/aarch64-ifetch/shelf-test.py \
		$(REGRESSION_TEST_MODE)
		@ echo "herd7 catalogue aarch64-ifetch tests: OK"


# Not in cata-test, too-long
test-all:: vmsa-test
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


#Too long to include in `make test-all`. Add -verbose option to reassure us that something is running
test-all-asl:: cata-asl-vmsa-test
cata-asl-vmsa-test: asl-pseudocode
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-verbose \
		-j $(J) \
		-herd-path $(HERD) \
		-herd-timeout $(TIMEOUT) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-VMSA/tests/VMSA-kinds.txt \
		-shelf-path catalogue/aarch64-VMSA/shelf.py \
		-conf-path catalogue/aarch64-VMSA/cfgs/asl.cfg \
		$(REGRESSION_TEST_MODE)
		@ echo "herd7 catalogue aarch64-VMSA tests: OK"

test-all:: cata-aarch64-cas-test
cata-aarch64-cas-test:
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-herd-timeout $(TIMEOUT) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-cas/tests/kinds.txt \
		-shelf-path catalogue/aarch64-cas/shelf.py \
		$(REGRESSION_TEST_MODE)
		@ echo "herd7 catalogue aarch64-cas tests: OK"

test-all:: ets2-test
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

test-all:: test.vmsa+mte
test.vmsa+mte:
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.vmsa+mte \
		-conf ./herd/tests/instructions/AArch64.vmsa+mte/vmsa+mte.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 VMSA+MTE instructions tests: OK"

### Diy tests, includes
### - A `diy7` with `cycleonly` instance checks the cycle generations
### - Several `diycross7` + `herd7` instances, check if the generated litmus tests
###   are equivalent based on `herd7` result.
diy-test:: | build
diy-test:: diy-baseline-cycleonly
diy-baseline-cycleonly::
	@ echo
	$(DIY_REGRESSION_TEST) \
		-diy-path $(DIY) \
		-conf ./gen/libdir/forbidden.conf \
		-expected ./gen/tests/baseline-size-4.cycle.expected \
		-diy-arg "-size" \
		-diy-arg "4" \
		$(REGRESSION_TEST_MODE)
	@ echo "diy7 baseline configuration test: OK"

diy-test:: diy-ifetch-cycleonly
diy-ifetch-cycleonly::
	@ echo
	$(DIY_REGRESSION_TEST) \
		-diy-path $(DIY) \
		-conf ./gen/libdir/forbidden_ifetch.conf \
		-expected ./gen/tests/ifetch.cycle.expected \
		$(REGRESSION_TEST_MODE)
	@ echo "diy7 ifetch configuration test: OK"

LDS:="Amo.Cas,Amo.LdAdd,Amo.LdClr,Amo.LdEor,Amo.LdSet"
LDSPLUS:="LxSx",$(LDS)

diy-test:: diy-test-aarch64
diy-test-aarch64:
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-diycross-path $(DIYCROSS) \
		-libdir-path ./herd/libdir \
		-expected-dir ./gen/tests/AArch64 \
		-diycross-arg -arch \
		-diycross-arg AArch64 \
		-diycross-arg 'A,L,P' \
		-diycross-arg 'Pod**,Fenced**,DSB.SYd**,ISBd**,[Amo.Cas,Pod**],[Amo.Swp,Pod**],[Amo.StAdd,Pod**],[LxSx,Pod**]' \
		-diycross-arg 'Rfe,Fre,Coe' \
		-diycross-arg 'DpAddrdR,DpAddrdW,DpDatadW,CtrldR,CtrldW,DpAddrCseldR,DpAddrCseldW,DpDataCseldW,DpCtrlCseldR,DpCtrlCseldW,[DpCtrldR,ISB],[DpCtrldW,ISB]' \
		-diycross-arg 'Rfe,Fre,Coe,Hat' \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 diycross7 tests: OK"

diy-test:: diy-test-mixed
diy-test-mixed::
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-diycross-path $(DIYCROSS) \
		-libdir-path ./herd/libdir \
		-expected-dir ./gen/tests/AArch64.mixed \
		-conf ./gen/tests/AArch64.mixed/mixed.cfg \
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
		-expected-dir ./gen/tests/AArch64.mixed.strict \
		-conf ./gen/tests/AArch64.mixed.strict/mixed.cfg \
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
		-expected-dir ./gen/tests/AArch64.mixed.v32 \
		-conf ./gen/tests/AArch64.mixed.strict/mixed.cfg \
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
		-expected-dir ./gen/tests/AArch64.mixed.v64 \
		-conf ./gen/tests/AArch64.mixed.strict/mixed.cfg \
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
		-expected-dir ./gen/tests/AArch64.store \
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
		-expected-dir ./gen/tests/AArch64.MTE \
		-conf ./gen/tests/AArch64.MTE/MTE.cfg \
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

diy-test::  diy-test-C
diy-test-C:
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-j $(J) \
		-herd-path $(HERD) \
		-diycross-path $(DIYCROSS) \
		-libdir-path ./herd/libdir \
		-expected-dir ./gen/tests/C \
		-conf ./gen/tests/C/C.cfg \
		-diycross-arg -arch \
                -diycross-arg C \
		-diycross-arg [Rlx,Coe,Rlx],[Rlx,Rfe,Rlx],[Rlx,Fre,Rlx],[Rlx,Hat,Rlx] \
                -diycross-arg PosRW,Fetch.Add,Exch \
                -diycross-arg Rlx \
                -diycross-arg PodW* \
		-diycross-arg [Rlx,Coe,Rlx],[Rlx,Rfe,Rlx],[Rlx,Fre,Rlx] \
                -diycross-arg Pod**,[Fetch.Add,Rlx,PodW*] \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 C diycross7 tests: OK"

### Diymicro test
diymicro-test:: | build

diymicro-test:: diymicro-test-aarch64
diymicro-test-aarch64:
	$(eval DIYMICRO_EDGES = $(shell $(DIYMICROENUM) -list-iico | sed -n 's/^iico\[\([^ ]*\).*/iico[\1]/p'))
	$(eval DIYMICRO_EDGES_ARG := $(foreach arg,$(DIYMICRO_EDGES),-diycross-arg $(arg)))
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-diycross-path $(DIYMICROENUM) \
		-libdir-path ./herd/libdir \
		-expected-dir ./gen/tests/diymicro/AArch64 \
		$(DIYMICRO_EDGES_ARG) \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 diymicro7 tests: OK"

diymicro-test:: diymicro-test-aarch64-asl
diymicro-test-aarch64-asl: asl-pseudocode
	$(eval DIYMICRO_EDGES = $(shell $(DIYMICROENUM) -list-iico | sed -n 's/^iico\[\([^ ]*\).*/iico[\1]/p'))
	$(eval DIYMICRO_EDGES_ARG := $(foreach arg,$(DIYMICRO_EDGES),-diycross-arg $(arg)))
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-diycross-path $(DIYMICROENUM) \
		-libdir-path ./herd/libdir \
		-expected-dir ./gen/tests/diymicro/AArch64 \
		-conf ./gen/tests/diymicro/AArch64/asl.cfg \
		-j $(J) \
		$(DIYMICRO_EDGES_ARG) \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 diymicro7 (ASL) tests: OK"

.PHONY: asl-pseudocode
asl-pseudocode:
	@ $(MAKE) -C herd/libdir/asl-pseudocode build

.PHONY: clean-asl-pseudocode
clean-asl-pseudocode:
	@ $(MAKE) -C herd/libdir/asl-pseudocode clean

.PHONY: asldoc
asldoc: Version.ml
	@ dune build -j $(J) --profile $(DUNE_PROFILE) $(BENTO) $(ASLREF)
	@ $(MAKE) $(MFLAGS) -C asllib/doc all BENTO=$(CURDIR)/$(BENTO) ASLREF=$(CURDIR)/$(ASLREF)

.PHONY: clean-asldoc
clean-asldoc:
	@ $(MAKE) $(MFLAGS) -C asllib/doc clean

.PHONY: type-check-asl
type-check-asl: Version.ml
	@ echo
	@ dune build -j $(J) --profile $(DUNE_PROFILE) $(ASLREF)
	@ $(MAKE) $(MFLAGS) -C herd/libdir/asl-pseudocode type-check ASLREF=$(CURDIR)/$(ASLREF)
	@ echo "ASLRef type-checking of published Arm ASL code: OK"

RUN_TESTS?=false
$(V).SILENT:
$(V)SILENTOPT=-s

include Makefile.x86_64
include Makefile.aarch64
