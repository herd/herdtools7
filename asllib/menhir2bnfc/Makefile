SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

DUNE_PROFILE := dev
DUNE_FLAGS := --root=$(CURDIR) --profile=$(DUNE_PROFILE)
DUNE := dune

BNFC_INSTALLED := $(shell which bnfc)

.PHONY: check_bnfc_installed build dune-runtest test-integration test

check_bnfc_installed:
ifndef BNFC_INSTALLED
	@ echo "Checking that bnfc is installed..."
	bnfc --version
endif

build: check_bnfc_installed
	ls $(CURDIR)
	$(DUNE) build $(DUNE_FLAGS)

dune-runtest: check_bnfc_installed
	$(DUNE) runtest $(DUNE_FLAGS) .

test-integration: build
	$(MAKE) -C tests/integration test

test: build dune-runtest test-integration
