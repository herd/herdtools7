ASLREF=../../_build/install/default/bin/aslref
LATEX=pdflatex
BIBTEX=bibtex

GENERATED_MACROS_TEX=generated_macros.tex

all: ASLReference.pdf

LATEXMK=latexmk

ASLReference.pdf: generated_spec *.tex *.bib
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
	dune exec -- aslspec asl.spec --render;

.PHONY: clean
clean:
	/bin/rm -f $(PDFS)
	/bin/rm -f $(GENERATED_MACROS_TEX)
	/bin/rm -f *.aux *.log *.fls *.log *.toc *.fdb_latexmk *~
	/bin/rm -f $(CONTROLS)
	/bin/rm -f comment.cut
	/bin/rm -f *.blg *.bbl *.out *.dvi *.synctex.gz
