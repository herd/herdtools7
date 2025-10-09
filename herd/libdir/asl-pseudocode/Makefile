.PHONY: default
default: a64

PYTHON := python3

BUNDLER_JOBS :=
BUNDLER_ARGS := -vv $(BUNDLER_JOBS)
BUNDLER := bundler.py
BUNDLER_CMD := $(PYTHON) $(BUNDLER) $(BUNDLER_ARGS)

BASE_URL := https://developer.arm.com/-/cdn-downloads/permalink/

ISA_A64_DIR := Exploration-Tools-A64-ISA/ISA_A64
ISA_A64_NAME := ISA_A64_xml_A_profile-2025-09_ASL0

TARGETS := $(ISA_A64_NAME)
TARGETS_TAR_GZ := $(addsuffix .tar.gz,$(TARGETS))
TARGETS_LOG := $(addsuffix .log,$(TARGETS))

# .SECONDARY does not support wildcards
.SECONDARY: $(TARGETS_TAR_GZ) $(TARGETS)

$(ISA_A64_NAME).tar.gz:
	@ echo "Downloading ISA $@ from developer.arm.com ..."
	curl $(BASE_URL)/$(ISA_A64_DIR)/$@ --silent --output $@

$(TARGETS): %: | %.tar.gz
	@ echo "Unpacking xml archive ..." $*.tar.gz
	@ tar -zxf $*.tar.gz $*

$(TARGETS_LOG): %.log: | %
	@ echo "Copying proprietary notice ..."
	@ cp $*/xhtml/notice.html .
	@ echo "Extracting ASL pseudocode from xml files ..."
	@ $(BUNDLER_CMD) -o $(@D) --log-file $@ $*
	@ echo "You can find extraction log at $@"

.PHONY: a64
a64: $(ISA_A64_NAME).log

.PHONY: all
all: a64 a32 regs

.PHONY: clean-tmp
clean-tmp:
	@ rm -fr $(TARGETS) $(TARGETS_TAR_GZ)

.PHONY: clean
clean: clean-tmp
	rm -fr aarch64 shared_pseudocode.asl other-instrs $(TARGETS_LOG)
