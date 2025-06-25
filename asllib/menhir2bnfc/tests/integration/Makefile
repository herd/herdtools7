SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

HERDTOOLS_SOURCE := $(abspath ../../../..)
MENHIRTOBNFC_SOURCE := ../..

DUNE_PROFILE := dev
DUNE_FLAGS := --root=$(abspath $(MENHIRTOBNFC_SOURCE)) --profile=$(DUNE_PROFILE)
DUNE := dune

ASLLIB_PARSER_CMLY := $(HERDTOOLS_SOURCE)/_build/default/asllib/Parser.cmly
export ASLLIB_PARSER_CMLY

ASLLIB_TESTS := $(HERDTOOLS_SOURCE)/asllib/tests
PARSER_FROM_SOURCE := _build/default/tests/integration/parser_cmp.exe
PARSER_CMP := $(MENHIRTOBNFC_SOURCE)/$(PARSER_FROM_SOURCE)

.PHONY: $(PARSER_CMP)
$(PARSER_CMP):
	$(DUNE) build $(DUNE_FLAGS) $(PARSER_FROM_SOURCE)

ASL_TESTS := $(wildcard $(HERDTOOLS_SOURCE)/asllib/tests/*.t/*.asl)
.INTERMEDIATE: parser_cmp.out
parser_cmp.out: $(ASL_TESTS) $(PARSER_CMP)
	(cd $(HERDTOOLS_SOURCE) && $(abspath $(PARSER_CMP)) -q -o $(abspath $@) asllib/tests)

.PHONY: test
test: parser_cmp.out parser_cmp.expected
	diff parser_cmp.out parser_cmp.expected

