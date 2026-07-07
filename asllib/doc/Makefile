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

html: generated_spec *.tex *.bib *.hva
	mkdir -p HTML
	@# The first run is expected to generate warnings because bibhva
	@# and the second hevea pass have not run, so we silence them via -s.
	hevea -fix0 -s -O -o HTML/ASL.html book.hva macros.hva ASLReference.tex
	bibhva HTML/ASL
	@# bibhva makes citations available after the first pass; run one more
	@# silent pass so the final unsilenced pass starts from stable references.
	hevea -fix0 -s -O -o HTML/ASL.html book.hva macros.hva ASLReference.tex
	hevea -fix -O -o HTML/ASL.html book.hva macros.hva ASLReference.tex
	cd HTML && hacha ASL.html

generated_spec: asl.spec
	dune exec -- aslspec asl.spec --render;

.PHONY: html clean
clean:
	/bin/rm -f $(PDFS)
	/bin/rm -f $(GENERATED_MACROS_TEX)
	/bin/rm -f *.aux *.log *.fls *.log *.toc *.fdb_latexmk *~
	/bin/rm -f $(CONTROLS)
	/bin/rm -f comment.cut
	/bin/rm -f *.blg *.bbl *.out *.dvi *.synctex.gz
	/bin/rm -rf HTML
