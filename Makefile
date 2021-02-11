PREFIX=$$HOME
D=dune
#For building with ocamlbuild set
#D=ocb

REGRESSION_TEST_MODE = test
# REGRESSION_TEST_MODE = promote
# REGRESSION_TEST_MODE = show

OPAM_DEPS = menhir

ifeq ($(D), dune)
	DIYCROSS                      = _build/install/default/bin/diycross7
	HERD                          = _build/install/default/bin/herd7
	HERD_REGRESSION_TEST          = _build/default/internal/herd_regression_test.exe
	HERD_DIYCROSS_REGRESSION_TEST = _build/default/internal/herd_diycross_regression_test.exe

	OPAM_DEPS += dune
else
	DIYCROSS                      = _build/gen/diycross.native
	HERD                          = _build/herd/herd.native
	HERD_REGRESSION_TEST          = _build/internal/herd_regression_test.native
	HERD_DIYCROSS_REGRESSION_TEST = _build/internal/herd_diycross_regression_test.native

	OPAM_DEPS += ocamlbuild ocamlfind
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


check-deps::
	@ command -v opam >/dev/null \
		|| (echo "Opam not installed; please install it from your system's package manager" \
			&& exit 1)

define check-opam-dep
check-deps::
	@ (opam list --installed --columns=name \
		| grep -E '^$(1)$$$$' >/dev/null) \
	|| (echo "Package $(1) not installed; please run: opam install $(1)" \
		&& exit 1)
endef
$(foreach dep,$(OPAM_DEPS),$(eval $(call check-opam-dep,$(dep))))


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
