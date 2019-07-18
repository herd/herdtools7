# Directory containing this Makefile
ROOT_DIR := $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
# Directory containing templates
TEMPLATE_DIR := $(ROOT_DIR)/tests/illustrative/templates
# List of all templates
TEMPLATE_FILES ?= $(notdir $(wildcard $(TEMPLATE_DIR)/*.litmus-template))
# Output directory for variants
VARIANTS_DIR := $(TEMPLATE_DIR)/variants

.PHONY: clean
clean:
	rm -rf c11popl15
	rm -rf $(VARIANTS_DIR)

GENERATE_LITMUS_CMD=$(TEMPLATE_DIR)/genlitmus.py \
	--input $(TEMPLATE_DIR)/$(template) \
	--output_base $(VARIANTS_DIR)/$(subst .litmus-template,,$(template));
variants: $(VARIANTS_DIR)
$(VARIANTS_DIR):
	@$(foreach template, $(TEMPLATE_FILES), $(GENERATE_LITMUS_CMD))

.PHONY: quick_regression
quick_regression:
	./regression.py --skip-fig6 --skip-non-std-appendixA --skip-variants

.PHONY: full_regression
full_regression: $(VARIANTS_DIR)
	./regression.py

.PHONY: quick_regression7
quick_regression7:
	./regression.py --herd7 --skip-fig6 --skip-non-std-appendixA --skip-variants

.PHONY: full_regression7
full_regression7: $(VARIANTS_DIR)
	./regression.py --herd7
