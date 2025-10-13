.PHONY: default
default: a64

PYTHON := python3

BUNDLER_JOBS :=
BUNDLER_ARGS := -vv $(BUNDLER_JOBS)
BUNDLER := bundler.py
BUNDLER_CMD := $(PYTHON) $(BUNDLER) $(BUNDLER_ARGS)

ARCHITECTURE_RELEASE = 2025-09

BASE_URL := https://developer.arm.com/-/cdn-downloads/permalink

ISA_A64_DIR := Exploration-Tools-A64-ISA/ISA_A64
ISA_A64_NAME := ISA_A64_xml_A_profile-$(ARCHITECTURE_RELEASE)_ASL0
ISA_A64_TGT := $(ISA_A64_DIR)/$(ISA_A64_NAME)
ISA_A64_OFILE := aarch64

SYSREGS_DIR := Exploration-Tools-Arm-Architecture-System-Registers/SysReg
SYSREGS_NAME := SysReg_xml_A_profile-$(ARCHITECTURE_RELEASE)_ASL0
SYSREGS_TGT := $(SYSREGS_DIR)/$(SYSREGS_NAME)
SYSREGS_OFILE := system_registers.asl

FEATURES_DIR := Exploration-Tools-Arm-Architecture-Features/AARCHMRS
FEATURES_NAME := Features.json
FEATURES_TGT := $(FEATURES_DIR)/AARCHMRS_A_profile_FAT-$(ARCHITECTURE_RELEASE)_ASL0
FEATURES_OFILE := features.asl

TARGETS := $(ISA_A64_TGT) $(SYSREGS_TGT)
TARGETS_TAR_GZ := $(addsuffix .tar.gz,$(TARGETS))
TARGETS_NAME := $(notdir $(TARGETS))
TARGETS_LOG := $(addsuffix .log,$(TARGETS_NAME))
TARGETS_OFILE := $(ISA_A64_OFILE) $(FEATURES_OFILE) $(SYSREGS_OFILE)

WANTED_A64_REGS := sctlr_el2 sctlr_el1 sctlr2_el2 tcr_el1 tcr_el2 oslsr_el1 hcr_el2 ttbr1_el1 ttbr0_el1 spsr_el1 spsr_el2 mair_el1 s2pir_el2 hpfar_el2
WANTED_A32_REGS := scr
WANTED_EXT_REGS := edscr
WANTED_REGS := $(addprefix AArch64-,$(WANTED_A64_REGS)) $(addprefix ext-,$(WANTED_EXT_REGS)) $(addprefix AArch32-,$(WANTED_A32_REGS))
WANTED_REGS_XML := $(addsuffix .xml, $(WANTED_REGS))

# .SECONDARY does not support wildcards
.SECONDARY: $(TARGETS_TAR_GZ) $(TARGETS_NAME)

$(TARGETS_TAR_GZ):
	mkdir -p $(dir $@)
	@ echo "Downloading $@ from developer.arm.com ..."
	curl $(BASE_URL)/$@ --silent --output $@

$(ISA_A64_NAME): $(ISA_A64_TGT).tar.gz
$(SYSREGS_NAME): $(SYSREGS_TGT).tar.gz

$(TARGETS_NAME):
	@ echo "Unpacking xml archive $^ ..."
	@ tar -zxf $^ $@

$(ISA_A64_NAME).log: $(ISA_A64_NAME)
	@ echo "Copying proprietary notice ..."
	@ cp $^/xhtml/notice.html .
	@ echo "Extracting ASL pseudocode from xml files ..."
	@ $(BUNDLER_CMD) --make-opns -o $(@D) --log-file $@ $^
	@ echo "You can find extraction log at $@"

$(SYSREGS_OFILE): $(SYSREGS_NAME)
	@ mkdir -p regs
	@ echo "Copying proprietary notice ..."
	@ cp $^/xhtml/notice.html $(@D)/regs
	@ echo "Extracting ASL pseudocode from xml files ..."
	@ touch $@
	@ $(BUNDLER_CMD) --make-regs -o $@ --log-file $(SYSREGS_NAME).log $(addprefix $^/,$(WANTED_REGS_XML))
	@ echo "You can find extraction log at $(SYSREGS_NAME).log"

.PHONY: a64
a64: $(ISA_A64_NAME).log

.PHONY: regs
regs: $(SYSREGS_OFILE)

.PHONY: a32
a32:

.PHONY: all
all: a64 a32 regs

.PHONY: clean-tmp
clean-tmp:
	rm -fr $(TARGETS) $(TARGETS_TAR_GZ) $(TARGETS_NAME)

.PHONY: clean
clean: clean-tmp
	rm -fr aarch64 shared_pseudocode.asl regs other-instrs $(TARGETS_LOG)
