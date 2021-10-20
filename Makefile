.PHONY: check-deps

PREFIX=$$HOME
D=dune
#For building with ocamlbuild set
#D=ocb

REGRESSION_TEST_MODE = test
# REGRESSION_TEST_MODE = promote
# REGRESSION_TEST_MODE = show

ifeq ($(D), dune)
	DIYCROSS                      = _build/install/default/bin/diycross7
	HERD                          = _build/install/default/bin/herd7
	HERD_REGRESSION_TEST          = _build/default/internal/herd_regression_test.exe
	HERD_DIYCROSS_REGRESSION_TEST = _build/default/internal/herd_diycross_regression_test.exe
	HERD_CATALOGUE_REGRESSION_TEST = _build/default/internal/herd_catalogue_regression_test.exe
else
	DIYCROSS                      = _build/gen/diycross.native
	HERD                          = _build/herd/herd.native
	HERD_REGRESSION_TEST          = _build/internal/herd_regression_test.native
	HERD_DIYCROSS_REGRESSION_TEST = _build/internal/herd_diycross_regression_test.native
	HERD_CATALOGUE_REGRESSION_TEST = _build/internal/herd_catalogue_regression_test.exe
endif


all: build

build: | check-deps
	sh ./$(D)-build.sh $(PREFIX)

install:
	sh ./$(D)-install.sh $(PREFIX)

uninstall:
	sh ./$(D)-uninstall.sh $(PREFIX)

clean: $(D)-clean
	rm -f Version.ml

ocb-clean:
	ocamlbuild -clean

dune-clean:
	dune clean

versions:
	@ sh ./version-gen.sh $(PREFIX)
	@ dune build --workspace dune-workspace.versions @all


# Dependencies.

check-deps::
	$(if $(shell which ocaml),,$(error "Could not find ocaml in PATH"))
	$(if $(shell which menhir),,$(error "Could not find menhir in PATH; it can be installed with `opam install menhir`."))

ifeq ($(D), dune)
check-deps::
	$(if $(shell which dune),,$(error "Could not find dune in PATH; it can be installed with `opam install dune`."))
else
check-deps::
	$(if $(shell which ocamlbuild),,$(error "Could not find ocamlbuild in PATH; it can be installed with `opam install ocamlbuild`."))
	$(if $(shell which ocamlfind),,$(error "Could not find ocamlfind in PATH; it can be installed with `opam install ocamlfind`."))
endif


# Tests.

test:: | build

test:: $(D)-test
	@ echo "OCaml unit tests: OK"

dune-test:
	@ echo
	dune runtest --profile=release

ocb-test:
	@ echo
	./ocb-test.sh

test::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64 \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 instructions tests: OK"

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

test::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.neon \
		-variant neon \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 NEON instructions tests: OK"

test::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/AArch64.MTE \
		-conf ./herd/tests/instructions/AArch64.MTE/mte.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 MTE instructions tests: OK"

test::
	@ echo
	$(HERD_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-litmus-dir ./herd/tests/instructions/C \
		-conf ./herd/tests/instructions/C/c.cfg \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 C instructions tests: OK"

test::
	@ echo
	$(HERD_DIYCROSS_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-diycross-path $(DIYCROSS) \
		-libdir-path ./herd/libdir \
		-expected-dir ./herd/tests/diycross/AArch64 \
		-arch AArch64 \
		-relaxlist 'Pod**,Fenced**' \
		-relaxlist 'Rfe,Fre,Coe' \
		-relaxlist 'Pod**,Fenced**,DpAddrdR,DpAddrdW,DpDatadW,CtrldR,CtrldW' \
		-relaxlist 'Rfe,Fre,Coe' \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 AArch64 diycross7 tests: OK"

J=2

test::
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-j $(J) \
		-herd-timeout 1.0 \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64/tests/kinds.txt \
		-shelf-path catalogue/aarch64/shelf.py \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64 tests: OK"

test::
	@ echo
	$(HERD_CATALOGUE_REGRESSION_TEST) \
		-herd-path $(HERD) \
		-libdir-path ./herd/libdir \
		-kinds-path catalogue/aarch64-mixed/tests/kinds.txt \
		-shelf-path catalogue/aarch64-mixed/shelf.py \
		$(REGRESSION_TEST_MODE)
	@ echo "herd7 catalogue aarch64-mixed tests: OK"
