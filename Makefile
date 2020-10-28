PREFIX=$$HOME
D=dune
#For building with ocamlbuild set
#D=ocb

ifeq ($(D), dune)
	HERD = _build/install/default/bin/herd7
	TEST_HERD = _build/default/internal/test_herd.exe
else
	HERD = _build/herd/herd.native
	TEST_HERD = _build/internal/test_herd.native
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
	$(TEST_HERD) -herd-path $(HERD) -libdir-path ./herd/libdir -litmus-dir ./herd/unittests/AArch64 test
