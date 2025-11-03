.PHONY: default
default: a64

MAKEFLAGS += -j3

PYTHON := python3

BUNDLER_JOBS :=
BUNDLER_ARGS := -vv $(BUNDLER_JOBS)
BUNDLER := bundler.py
BUNDLER_CMD := $(PYTHON) $(BUNDLER) $(BUNDLER_ARGS)

ARCHITECTURE_RELEASE = 2025-09

BASE_URL := https://developer.arm.com/-/cdn-downloads/permalink

ISA_A64_DIR := Exploration-Tools-A64-ISA/ISA_A64
ISA_A64_NAME := ISA_A64_xml_A_profile-$(ARCHITECTURE_RELEASE)_ASL0
ISA_A64_OFILE := shared_pseudocode.asl
ISA_A64_TAR_GZ := $(ISA_A64_NAME).tar.gz

SYSREGS_DIR := Exploration-Tools-Arm-Architecture-System-Registers/SysReg
SYSREGS_NAME := SysReg_xml_A_profile-$(ARCHITECTURE_RELEASE)_ASL0
SYSREGS_OFILE := system_registers.asl
SYSREGS_TAR_GZ := $(SYSREGS_NAME).tar.gz

FEATURES_DIR := Exploration-Tools-Arm-Architecture-Features/AARCHMRS
FEATURES_NAME := Features.json
FEATURES_OFILE := features.asl
FEATURES_TAR_GZ := AARCHMRS_A_profile_FAT-$(ARCHITECTURE_RELEASE)_ASL0.tar.gz

TARGETS_TAR_GZ := $(ISA_A64_TAR_GZ) $(SYSREGS_TAR_GZ) $(FEATURES_TAR_GZ)
TARGETS_NAME := $(ISA_A64_NAME) $(SYSREGS_NAME) $(FEATURES_NAME)
TARGETS_LOG := $(addsuffix .log,$(TARGETS_NAME))
TARGETS_OFILE := $(ISA_A64_OFILE) $(FEATURES_OFILE) $(SYSREGS_OFILE)
TARGETS_DIR := $(ISA_A64_DIR) $(SYSREGS_DIR) $(FEATURES_DIR)

WANTED_A64_REGS := sctlr_el2 sctlr_el1 sctlr2_el2 tcr_el1 tcr_el2 oslsr_el1 hcr_el2 ttbr1_el1 ttbr0_el1 spsr_el1 spsr_el2 mair_el1 s2pir_el2 hpfar_el2
WANTED_A32_REGS := scr
WANTED_EXT_REGS := edscr
WANTED_REGS := $(addprefix AArch64-,$(WANTED_A64_REGS)) $(addprefix ext-,$(WANTED_EXT_REGS)) $(addprefix AArch32-,$(WANTED_A32_REGS))
WANTED_REGS_XML := $(addsuffix .xml, $(WANTED_REGS))

.INTERMEDIATE: $(TARGETS_TAR_GZ)

# Like intermediate, but doesn't try to remove folders
.SECONDARY: $(TARGETS_NAME)

# Trick with a phony order-only dependency to be able to pass an argument to a
# recipe shared between multiple targets
.PHONY: $(TARGETS_DIR)
$(TARGETS_DIR):

$(ISA_A64_TAR_GZ): | $(ISA_A64_DIR)
$(SYSREGS_TAR_GZ): | $(SYSREGS_DIR)
$(FEATURES_TAR_GZ): | $(FEATURES_DIR)

$(TARGETS_TAR_GZ):
	@ echo "Downloading $@ from developer.arm.com ..."
	@ curl --silent $(BASE_URL)/$|/$@ --output $@

$(ISA_A64_NAME): $(ISA_A64_TAR_GZ)
$(SYSREGS_NAME): $(SYSREGS_TAR_GZ)
$(FEATURES_NAME): $(FEATURES_TAR_GZ)

$(TARGETS_NAME):
	@ echo "Unpacking xml archive $^ ..."
	@ tar -zxf $^ $@

$(ISA_A64_OFILE): $(ISA_A64_NAME)
	@ cp $^/xhtml/notice.html .
	@ echo "Extracting instructions ASL pseudocode from xml files ..."
	@ $(BUNDLER_CMD) --make-opns -o $(@D) --log-file $^.log $^
	@ echo "You can find extraction log at $^.log"

$(SYSREGS_OFILE): $(SYSREGS_NAME)
	@ cp $^/xhtml/notice.html $(@D)/registers-notice.html
	@ echo "Extracting system registers ASL pseudocode from xml files ..."
	@ touch $@
	@ $(BUNDLER_CMD) --make-regs -o $@ --log-file $^.log $(addprefix $^/,$(WANTED_REGS_XML))
	@ echo "You can find extraction log at $^.log"

$(FEATURES_OFILE): $(FEATURES_NAME)
	@ echo "Extracting Features type definition from json file $^"
	@ $(PYTHON) build_features.py -o $@ $^

.PHONY: a64
a64: $(ISA_A64_OFILE)

.PHONY: regs
regs: $(SYSREGS_OFILE)

.PHONY: a32
a32:

.PHONY: all
all: a64 a32 regs $(FEATURES_OFILE)

# Ensure that the content of .INTERMEDIATE is empty
.PHONY: clean-tmp
clean-tmp: | all
	rm -fr $(TARGETS_NAME)

.PHONY: clean
clean: clean-tmp
	rm -fr aarch64 other-instrs $(TARGETS_LOG) $(TARGETS_OFILE) notice.html registers-notice.html
