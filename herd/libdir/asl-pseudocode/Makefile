# SPDX-FileCopyrightText: Copyright 2022-2023 Arm Limited and/or its affiliates <open-source-office@arm.com>
# SPDX-License-Identifier: BSD-3-Clause

# In order to run the build configuration, there are 3 maximum independent
# threads that needs to be used. We thus hardcode this number of threads.
MAKEFLAGS += -j3

# This variable stores all the file names built by the targets in this file,
# that will need to be removed by `make clean`
TO_CLEANUP =

PYTHON := python3

BUNDLER_JOBS :=
BUNDLER_ARGS := -vv $(BUNDLER_JOBS)
BUNDLER := bundler.py
BUNDLER_CMD := $(PYTHON) $(BUNDLER) $(BUNDLER_ARGS)

ARCHITECTURE_RELEASE = 2025-12

BASE_URL := https://developer.arm.com/-/cdn-downloads/permalink

ISA_A64_DIR := Exploration-Tools-A64-ISA/ISA_A64
ISA_A64_NAME := ISA_A64_xml_A_profile-$(ARCHITECTURE_RELEASE)
ISA_A64_OFILE := aarch64
ISA_A64_TAR_GZ := $(ISA_A64_NAME).tar.gz

SYSREGS_DIR := Exploration-Tools-Arm-Architecture-System-Registers/SysReg
SYSREGS_NAME := SysReg_xml_A_profile_FAT-$(ARCHITECTURE_RELEASE)
SYSREGS_OFILE := system_registers.asl
SYSREGS_TAR_GZ := $(SYSREGS_NAME).tar.gz

FEATURES_DIR := Exploration-Tools-Arm-Architecture-Features/AARCHMRS
FEATURES_NAME := Features.json
FEATURES_OFILE := features.asl
FEATURES_TAR_GZ := AARCHMRS_A_profile_FAT-$(ARCHITECTURE_RELEASE).tar.gz

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
TO_CLEANUP +=  $(TARGETS_TAR_GZ)

# Like intermediate, but doesn't try to remove folders
.SECONDARY: $(TARGETS_NAME)
TO_CLEANUP += $(TARGETS_NAME)

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

TO_CLEANUP += notice.html shared_pseudocode.asl shared_pseudocode.asl.log

shared_pseudocode.asl: $(ISA_A64_NAME)
	@ cp $^/xhtml/notice.html .
	@ echo "Extracting the shared ASL pseudocode from xml files ..."
	@ touch $@
	@ $(BUNDLER_CMD) --make-shared-pseudocode -o $@ --log-file $@.log $^/shared_pseudocode.xml
	@ echo "You can find extraction log at $@.log"

TO_CLEANUP += aarch64-notice.html $(ISA_A64_NAME).log $(ISA_A64_OFILE)

$(ISA_A64_OFILE): $(ISA_A64_NAME)
	@ cp $^/xhtml/notice.html $(@D)/aarch64-notice.html
	@ echo "Extracting instructions ASL pseudocode from xml files ..."
	@ $(BUNDLER_CMD) --make-opns -o $(@D) --log-file $^.log $^
	@ echo "You can find extraction log at $^.log"

TO_CLEANUP += registers-notice.html $(SYSREGS_NAME).log $(SYSREGS_OFILE)

$(SYSREGS_OFILE): $(SYSREGS_NAME)
	@ cp $^/xhtml/notice.html $(@D)/registers-notice.html
	@ echo "Extracting system registers ASL pseudocode from xml files ..."
	@ touch $@
	@ $(BUNDLER_CMD) --make-regs -o $@ --log-file $^.log $(addprefix $^/,$(WANTED_REGS_XML))
	@ echo "You can find extraction log at $^.log"

TO_CLEANUP += $(FEATURES_OFILE) $(FEATURES_NAME)

$(FEATURES_OFILE): $(FEATURES_NAME)
	@ echo "Extracting Features type definition from json file $^"
	@ $(PYTHON) build_features.py -o $@ $^

.PHONY: a64
a64: $(ISA_A64_OFILE) shared_pseudocode.asl

.PHONY: regs
regs: $(SYSREGS_OFILE)

.PHONY: a32
a32:

.PHONY: features
features: $(FEATURES_OFILE)

.PHONY: just-build
just-build: a64 a32 regs features

# The following variable control the production of the ASL code for the system
# registers. They are inferred from the error that we get when we try to
# type-check the code in shared_pseudocode.asl without any exceptional
# behaviour for the system registers.

# ERRGSR<m> and CNTVOFF<n> have naming clashes, and the indexed version are not
# used by shared_pseudocode.asl
# TRMPAM_EL1 is defined in the committed file `tbbmpam_el1.asl`, which is
# edited by hand to include a field that is not supported by `bundler.py`.
IGNORE_REGS := $\
		ERRGSR<m>,$\
		CNTVOFF<n>,$\
		TRBMPAM_EL1

# The following registers are the ones for which we get an error of the form
#   the type {REGISTER_NAME}_TYPE does not have field Foo.
# In that case, we add the register to this list
ALLOW_CONDITIONAL_FIELDS_FOR := $\
		CONTEXTIDR,$\
		EDVIDSR,$\
		MPAM0_EL1,$\
		MPAM1_EL1,$\
		MPAM2_EL2,$\
		MPAM3_EL3,$\
		MPAMHCR_EL2,$\
		MPAMSM_EL1,$\
		PAR_EL1,$\
		PMCNTENSET_EL0,$\
		PMINTENSET_EL1,$\
		PMCR_EL0,$\
		TCR2_EL2,$\
		TCR2MASK_EL2,$\
		TTBCR,$\
		TTBR0,$\
		TTBR0_EL1,$\
		TTBR0_EL2,$\
		TTBR0_EL3,$\
		TTBR1,$\
		TTBR1_EL1,$\
		TTBR1_EL2,$\
		VSESR_EL2,$\
		VSTTBR_EL2,$\
		VTTBR_EL2

# The following registers have fields that are at different places depending on
# configuration or runtime state. Because they are of the same length, it does
# not make a lot of difference which one we choose for type-checking. This
# configuration item instruct `bundler.py` to take one of the possible
# positions.
ALLOW_MOVING_FIELDS_FOR := $\
		CPTR_EL2,$\
		CPTRMASK_EL2,$\
		TCR_EL2,$\
		TCRMASK_EL2

# The following registers have fields that are at different places and of
# different length depending on configuration or runtime state. This
# configuration instruct `bundler.py` to use the biggest.
ALLOW_INCLUDED_FIELDS_FOR := $\
		PAR,$\
		RGSR_EL1,$\
		TRBDEVID1,$\
		MPAMIDR_EL1

# The following registers have names that clash with some declarations in
# shared_pseudocode.asl. We thus instruct `bundler.py` not to build the
# clashing accessor declarations.
DO_NOT_GENERATE_VARIABLES_FOR := $\
		BRBTGT<n>_EL1,$\
		BRBSRC<n>_EL1,$\
		BRBINF<n>_EL1,$\
		SVCR

TO_CLEANUP += all-registers-notice.html all_system_registers.asl $(SYSREGS_NAME).log

all_system_registers.asl: $(SYSREGS_NAME)
	@ cp $^/xhtml/notice.html $(@D)/all-registers-notice.html
	@ echo "Extracting system registers ASL pseudocode from xml files ..."
	@ touch $@
	@ $(BUNDLER_CMD) --make-regs -o $@ --log-file $^.log \
		--allow-optional-fields-for "$(ALLOW_CONDITIONAL_FIELDS_FOR)" \
		--allow-moving-fields-for "$(ALLOW_MOVING_FIELDS_FOR)" \
		--allow-included-fields-for "$(ALLOW_INCLUDED_FIELDS_FOR)" \
		--do-not-generate-variables-for "$(DO_NOT_GENERATE_VARIABLES_FOR)" \
		--ignored-regs "$(IGNORE_REGS)" \
    $^
	@ echo "You can find extraction log at $^.log"

TO_CLEANUP += instructions-notice.html all_instructions.asl $(ISA_A64_NAME).log

all_instructions.asl: $(ISA_A64_NAME)
	@ cp $^/xhtml/notice.html $(@D)/instructions-notice.html
	@ echo "Extracting instructions ASL pseudocode from xml files ..."
	@ touch $@
	@ $(BUNDLER_CMD) --make-funs -o $@ --log-file $^.log $^
	@ echo "You can find extraction log at $^.log"

TO_CLEANUP += pmu_event_types.asl

pmu_event_types.asl: shared_pseudocode.asl
	@ grep "\\<PMU_\\w\\+" shared_pseudocode.asl -o | sort | uniq | sed "s/\(.*\)$$/constant \\1 = Zeros{16};/" > $@

ASLREF := aslref

.PHONY: type-check
type-check: $(FEATURES_OFILE) all_system_registers.asl all_instructions.asl shared_pseudocode.asl type-checking-implementations.asl pmu_event_types.asl trbmpam_el1.asl
	$(ASLREF) --no-exec $^


# Ensure that the content of .INTERMEDIATE is empty
.PHONY: clean-tmp
clean-tmp: | just-build
	rm -fr $(TARGETS_NAME)

.DEFAULT_GOAL = build
.PHONY: build
build: just-build clean-tmp

.PHONY: clean
clean:
	rm -fr other-instrs $(TO_CLEANUP)
