BENTO=../../_build/default/tools/bento.exe
ASLREF=../../_build/install/default/bin/aslref
LATEX=pdflatex
BIBTEX=bibtex

PDFS=ASLReference.pdf

all: $(PDFS)

short:
	$(MAKE) $(MFLAGS) NOEMPTY=1 all

code:
	$(MAKE) $(MFLAGS) CODE=1 all

no-empty:
	$(MAKE) $(MFLAGS) NOEMPTY=1 all

CONTROLS=ifempty.tex ifcode.tex

.PHONY: control
control: control.tex $(CONTROLS)

LATEXMK=latexmk

ASLReference.pdf: control\
				ASLASTLines.tex ASLTypeSatisfactionLines.tex ASLStaticModelLines.tex ASLStaticInterpreterLines.tex\
				ASLStaticEnvLines.tex ASLEnvLines.tex ASLASTUtilsLines.tex\
				ASLTypingLines.tex ASLSemanticsLines.tex ASLStaticOperationsLines.tex\
				desugarLines.tex
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
	python3 doclint.py -m --aslref $(ASLREF)

ASLASTLines.tex: ../AST.mli
	$(BENTO) $< > $@

ASLTypeSatisfactionLines.tex: ../types.ml
	$(BENTO) $< > $@

ASLEnvLines.tex: ../env.ml
	$(BENTO) $< > $@

ASLSemanticsLines.tex: ../Interpreter.ml
	$(BENTO) $< > $@

desugarLines.tex: ../desugar.ml
	$(BENTO) $< > $@

ASL%Lines.tex: ../%.ml
	$(BENTO) $< > $@

.PHONY: force

ifdef CODE
ifcode.tex: force
	@echo "\\\\newif\\ifcode\\\\codetrue" >$@
else
ifcode.tex: force
	@echo "\\\\newif\\ifcode\\\\codefalse" >$@
endif

ifdef SHOW_EMPTY
ifempty.tex: force
	@echo "\\\\newif\\ifempty\\\\emptytrue" >$@
else
ifempty.tex: force
	@echo "\\\\newif\\ifempty\\\\emptyfalse" >$@
endif

.PHONY: clean
clean:
	/bin/rm -f $(PDFS)
	/bin/rm -f *Lines.tex
	/bin/rm -f *.aux *.log *.fls *.log *.toc *.fdb_latexmk *~
	/bin/rm -f $(CONTROLS)
	/bin/rm -f comment.cut
	/bin/rm -f *.blg *.bbl *.out
