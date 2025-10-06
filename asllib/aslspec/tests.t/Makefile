# Makefile for promoting test expected files:
# Run `make promote` to regenerate expected files

.PHONY: promote clean

SPECS = hello.spec typedefs.spec relations.spec
EXPECTED = $(SPECS:.spec=_macros.expected)

promote: $(EXPECTED)

%_macros.expected: %.spec
	dune exec -- aslspec $< --render
	mv generated_macros.tex $@
	rm -f generated_macros.tex

clean:
	rm -f generated_macros.tex
