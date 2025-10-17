# Makefile for promoting test expected files:
# Run `make promote` to regenerate expected files

.PHONY: promote

# Find all .expected files and get their corresponding .spec files
EXPECTED := $(wildcard *.expected)
SPECS_TO_PROMOTE := $(EXPECTED:.expected=.spec)

promote:
	rm -f *.expected
	for spec in $(SPECS_TO_PROMOTE); do \
		echo "Promoting $$spec"; \
		dune exec -- aslspec $$spec --render; \
		mv generated_macros.tex $${spec%.spec}.expected; \
		rm -f generated_macros.tex; \
	done
