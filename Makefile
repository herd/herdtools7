PREFIX=$$HOME
D=dune
#For building with ocamlbuild set
#D=ocb

ifeq ($(D), dune)
	HERD = _build/install/default/bin/herd7
else
	HERD = _build/herd/herd.native
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

test: regression-tests
	@ echo "Tests OK."

regression-tests: AArch64-regression-tests

REGRESSION_TEST_DIR = _build/regression-tests
AARCH64_REGRESSION_TEST_LITMUSES = $(shell find herd/unittests/AArch64 -type f -name '*.litmus')
AARCH64_REGRESSION_TEST_OUTPUTS  = $(patsubst %.litmus,$(REGRESSION_TEST_DIR)/%.litmus.expected,$(AARCH64_REGRESSION_TEST_LITMUSES))

AArch64-regression-tests: $(AARCH64_REGRESSION_TEST_OUTPUTS)
	@ $(foreach output,$^,(diff --new-file $(output) $(patsubst $(REGRESSION_TEST_DIR)/%.litmus.expected,%.litmus.expected,$(output)) || (echo $(output); exit 1)) && ) true

$(REGRESSION_TEST_DIR)/%.litmus.expected: %.litmus | build
	@ mkdir -p $(@D)
	$(HERD) -set-libdir ./herd/libdir $^ | grep -v -E '^Time' > $@
