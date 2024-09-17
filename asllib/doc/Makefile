BENTO=../../_build/default/tools/bento.exe
LATEX=pdflatex
BIBTEX=bibtex

PDFS=ASLSyntaxReference.pdf ASLSemanticsReference.pdf ASLTypingReference.pdf ASLRefProgress.pdf

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

ASLSyntaxReference.pdf: control ASLASTLines.tex ASLSyntaxReference.tex
	$(LATEX) ASLSyntaxReference.tex
	$(LATEX) ASLSyntaxReference.tex
	$(LATEX) ASLSyntaxReference.tex

ASLSemanticsReference.pdf: control ASLSyntaxReference.pdf ASLSemanticsReference.tex ASLSemanticsLines.tex
	$(LATEX) ASLSemanticsReference.tex
	$(BIBTEX) ASLSemanticsReference
	$(LATEX) ASLSemanticsReference.tex
	$(BIBTEX) ASLSemanticsReference
	$(LATEX) ASLSemanticsReference.tex

ASLTypingReference.pdf: control ASLSyntaxReference.pdf ASLTypingReference.tex ASLASTLines.tex ASLTypingLines.tex ASLTypeSatisfactionLines.tex ASLStaticModelLines.tex ASLStaticInterpreterLines.tex ASLStaticEnvLines.tex ASLASTUtilsLines.tex
	$(LATEX) ASLTypingReference.tex
	$(BIBTEX) ASLTypingReference
	$(LATEX) ASLTypingReference.tex
	$(BIBTEX) ASLTypingReference
	$(LATEX) ASLTypingReference.tex

ASLRefProgress.pdf: ASLRefProgress.tex
	$(LATEX) $<
	$(LATEX) $<
	$(LATEX) $<

ASLSemanticsLines.tex: ../Interpreter.ml
	$(BENTO) $< > $@

ASLTypingLines.tex: ../Typing.ml
	$(BENTO) $< > $@

ASLTypeSatisfactionLines.tex: ../types.ml
	$(BENTO) $< > $@

ASLASTLines.tex: ../AST.mli
	$(BENTO) $< > $@

ASLStaticModelLines.tex: ../StaticModel.ml
	$(BENTO) $< > $@

ASLStaticInterpreterLines.tex: ../StaticInterpreter.ml
	$(BENTO) $< > $@

ASLStaticEnvLines.tex: ../StaticEnv.ml
	$(BENTO) $< > $@

ASLASTUtilsLines.tex: ../ASTUtils.ml
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

clean:
	/bin/rm -f $(PDFS)
	/bin/rm -f ASLSemanticsLines.tex ASLTypingLines.tex ASLTypeSatisfactionLines.tex ASLASTLines.tex ASLStaticModelLines.tex ASLStaticInterpreterLines.tex ASLStaticEnvLines.tex ASLASTUtilsLines.tex
	/bin/rm -f *.aux *.log *.fls *.log *.toc *.fdb_latexmk *~
	/bin/rm -f $(CONTROLS)
	/bin/rm -f comment.cut
	/bin/rm -f *.blg *.bbl *.out
