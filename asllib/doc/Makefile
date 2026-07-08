ASLREF=../../_build/install/default/bin/aslref
LATEX=pdflatex
BIBTEX=bibtex

GENERATED_MACROS_TEX=generated_macros.tex
HEVEA_DIR=hevea
MATHJAX_CONFIG_JS=HTML/ASL-mathjax-config.js
HTML_STYLESHEET=$(HEVEA_DIR)/asl-html.css
HEVEA_HTML_INPUTS=book.hva mathjaxauto.hva $(HEVEA_DIR)/mathjax_config.hva macros.hva ASLReference.tex

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

html: generated_spec *.tex *.bib *.hva $(HEVEA_DIR)/*.hva $(HEVEA_DIR)/generate_mathjax_config.py $(HTML_STYLESHEET)
	mkdir -p HTML
	cp $(HTML_STYLESHEET) HTML/asl-html.css
	@# The first run is expected to generate warnings because bibhva
	@# and the second hevea pass have not run, so we silence them via -s.
	@# MathJax mode intentionally omits -O: Esponja does not understand the raw
	@# TeX fragments that HeVeA leaves for MathJax to typeset in the browser.
	hevea -fix0 -s -o HTML/ASL.html $(HEVEA_HTML_INPUTS)
	bibhva HTML/ASL
	@# bibhva makes citations available after the first pass; run one more
	@# silent pass so the final unsilenced pass starts from stable references.
	hevea -fix0 -s -o HTML/ASL.html $(HEVEA_HTML_INPUTS)
	hevea -fix -o HTML/ASL.html $(HEVEA_HTML_INPUTS)
	python3 $(HEVEA_DIR)/generate_mathjax_config.py $(MATHJAX_CONFIG_JS) HTML/ASL.html
	cd HTML && hacha ASL.html
	python3 $(HEVEA_DIR)/generate_mathjax_config.py $(MATHJAX_CONFIG_JS) HTML/ASL[0-9][0-9][0-9].html

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
