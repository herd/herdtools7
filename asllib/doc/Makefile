ASLREF=../../_build/install/default/bin/aslref
LATEX=pdflatex
BIBTEX=bibtex

PDFS=ASLReference.pdf

GENERATED_ELEMENTS_TEX=generated_elements.tex

all: generated_spec $(PDFS)

LATEXMK=latexmk

ASLReference.pdf:
	if command -v $(LATEXMK) >/dev/null 2>&1; \
	then \
		$(LATEXMK) -pdf ASLReference.tex; \
	else \
		$(LATEX) ASLReference.tex; \
		$(BIBTEX) ASLReference; \
		$(LATEX) ASLReference.tex; \
		$(BIBTEX) ASLReference; \
		$(LATEX) ASLReference.tex; \
	fi
	python3 doclint.py --console_macros --aslref $(ASLREF)

generated_spec: asl.spec
	dune exec -- aslspec --render asl.spec;

.PHONY: clean
clean:
	/bin/rm -f $(PDFS)
	/bin/rm -f $(GENERATED_ELEMENTS_TEX)
	/bin/rm -f *.aux *.log *.fls *.log *.toc *.fdb_latexmk *~
	/bin/rm -f $(CONTROLS)
	/bin/rm -f comment.cut
	/bin/rm -f *.blg *.bbl *.out *.dvi *.synctex.gz
