.PHONY: default
default: a64

PYTHON := python3

BUNDLER_JOBS :=
BUNDLER_ARGS := -vv $(BUNDLER_JOBS)
BUNDLER := bundler.py
BUNDLER_CMD := $(PYTHON) $(BUNDLER) $(BUNDLER_ARGS)

BASE_URL := https://developer.arm.com/-/media/developer/products/architecture/armv9-a-architecture/2022-12/

ISA_A64_NAME := ISA_A64_xml_A_profile-2022-12
ISA_A32_NAME := ISA_AArch32_xml_A_profile-2022-12
REGS_NAME := SysReg_xml_A_profile-2022-12

STAGING_DIR := build

$(STAGING_DIR):
	@ mkdir -p $@

# .SECONDARY does not support wildcards
.SECONDARY: $(STAGING_DIR)/$(ISA_A64_NAME).tar.gz $(STAGING_DIR)/$(ISA_A32_NAME).tar.gz $(STAGING_DIR)/$(REGS_NAME).tar.gz
.SECONDARY: $(STAGING_DIR)/$(ISA_A64_NAME) $(STAGING_DIR)/$(ISA_A32_NAME) $(STAGING_DIR)/$(REGS_NAME)

$(STAGING_DIR)/%.tar.gz: $(STAGING_DIR)
	@ echo "Downloading ISA from developer.arm.com ..."
	@ curl $(BASE_URL)/$*.tar.gz --silent --output $@

$(STAGING_DIR)/%: $(STAGING_DIR)/%.tar.gz
	@ echo "Unpacking xml archive ..."
	@ tar -zxf $< --directory=$(STAGING_DIR)

%.log: $(STAGING_DIR)/%
	@ echo "Copying proprietary notice ..."
	@ cp $^/xhtml/notice.html .
	@ echo "Extracting ASL pseudocode from xml files ..."
	@ $(BUNDLER_CMD) -o $(@D) --log-file $@ $^
	@ echo "You can find extraction log at $@"

.PHONY: a64 a32 regs
a64: $(ISA_A64_NAME).log
a32: $(ISA_A32_NAME).log
regs: $(REGS_NAME).log

.PHONY: all
all: a64 a32 regs

.PHONY: clean-tmp
clean-tmp:
	@ rm -fr $(STAGING_DIR)

.PHONY: clean
clean: clean-tmp
	rm -fr aarch64 shared_pseudocode.asl other-instrs $(ISA_A64_NAME).log
