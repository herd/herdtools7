ASLREF=../../_build/default/asllib/aslref.exe
LATEX=pdflatex
BIBTEX=bibtex

GENERATED_MACROS_TEX=generated_macros.tex
GENERATED_CONSOLE_MACROS_TEX=generated_console_macros.tex
ASL_TESTS=$(shell find ../tests -name '*.asl')

# HeVeA and Hacha can emit warnings on stderr while still exiting with success.
# Capture stderr from the command, preserve real command failures, and make
# warning diagnostics fatal.
# Match both "Warning:" and "Warning," patterns because the tools use both forms.
# We don't match just "Warning" to ignore ordinary text containing that word.
# Argument 1: command to run.
# Argument 2: tool name for the failure message.
ASL_HTML_FAIL_ON_STDERR_WARNING = messages="$$($1 2>&1)"; \
	rc="$$?"; \
	: "Catch tool failure."; \
	if [ "$$rc" -ne 0 ]; then \
		printf '%s\n' "$$messages"; \
		exit "$$rc"; \
	fi; \
	: "Detect warning message patterns."; \
	if printf '%s\n' "$$messages" | grep -Eq "Warning[:,]"; then \
		printf '%s\n' "$$messages"; \
		echo "$2 warnings detected; failing HTML build."; \
		exit 1; \
	fi

all: ASLReference.pdf

LATEXMK=latexmk

ASLReference.pdf: generated_spec $(GENERATED_CONSOLE_MACROS_TEX) *.tex *.bib
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
	python3 doclint.py

$(GENERATED_CONSOLE_MACROS_TEX): extended_macros.py doclint.py $(ASLREF) $(ASL_TESTS) $(filter-out $(GENERATED_CONSOLE_MACROS_TEX),$(wildcard *.tex))
	python3 doclint.py --console-macros-only --aslref $(ASLREF)

$(ASLREF):
	@# Build the aslref target in the parent asllib directory.
	dune build ../aslref.exe

html: generated_spec $(GENERATED_CONSOLE_MACROS_TEX) *.tex *.bib *.hva
	mkdir -p HTML
	@# The first run is expected to generate warnings because bibhva
	@# and the second hevea pass have not run, so we silence them via -s.
	hevea -fix0 -s -O -o HTML/ASL.html book.hva macros.hva ASLReference.tex
	bibhva HTML/ASL
	@# bibhva makes citations available after the first pass; run one more
	@# silent pass so the final unsilenced pass starts from stable references.
	hevea -fix0 -s -O -o HTML/ASL.html book.hva macros.hva ASLReference.tex
	@echo "hevea -fix -O -o HTML/ASL.html book.hva macros.hva ASLReference.tex"
	@$(call ASL_HTML_FAIL_ON_STDERR_WARNING,hevea -fix -O -o HTML/ASL.html book.hva macros.hva ASLReference.tex,HeVeA)
	@echo "cd HTML && hacha ASL.html"
	@$(call ASL_HTML_FAIL_ON_STDERR_WARNING,cd HTML && hacha ASL.html,Hacha)

generated_spec: asl.spec
	dune exec -- aslspec asl.spec --render;

.PHONY: html clean
clean:
	/bin/rm -f $(PDFS)
	/bin/rm -f $(GENERATED_MACROS_TEX)
	/bin/rm -f $(GENERATED_CONSOLE_MACROS_TEX)
	/bin/rm -rf generated_console_output
	/bin/rm -f *.aux *.log *.fls *.log *.toc *.fdb_latexmk *~
	/bin/rm -f $(CONTROLS)
	/bin/rm -f comment.cut
	/bin/rm -f *.blg *.bbl *.out *.dvi *.synctex.gz
	/bin/rm -rf HTML
