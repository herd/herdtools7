PREFIX=$$HOME
D=dune
#For building with ocamlbuild set
#D=ocb

ifeq ($(D), dune)
	HERD = _build/install/default/bin/herd7
	HERD_REGRESSION_TEST = _build/default/internal/herd_regression_test.exe
else
	HERD = _build/herd/herd.native
	HERD_REGRESSION_TEST = _build/internal/herd_regression_test.native
endif

all: build

build:
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


# Tests.

test:: | build

test:: $(D)-test

dune-test:
	dune runtest

ocb-test:
	./ocb-test.sh

test::
	$(HERD_REGRESSION_TEST) -herd-path $(HERD) -libdir-path ./herd/libdir -litmus-dir ./herd/unittests/AArch64 test
