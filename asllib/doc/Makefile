BENTO=../../_build/default/tools/bento.exe
LATEX=pdflatex
BIBTEX=bibtex

PDFS=ASLSyntaxReference.pdf ASLSemanticsReference.pdf ASLTypingReference.pdf ASLRefProgress.pdf

all: $(PDFS)

short:
	$(MAKE) $(MFLAGS) NOEMPTY=1 NOFORMAL=1 all

no-formal:
	$(MAKE) $(MFLAGS) NOFORMAL=1 all

no-empty:
	$(MAKE) $(MFLAGS) NOEMPTY=1 all

todo:
	$(MAKE) $(MFLAGS) TODO=1 all

CONTROLS=ifempty.tex ifformal.tex iftodo.tex

.PHONY: control
control: control.tex $(CONTROLS)

ASLSyntaxReference.pdf: control ASLASTLines.tex ASLSyntaxReference.tex
	$(LATEX) ASLSyntaxReference.tex
	$(LATEX) ASLSyntaxReference.tex
	$(LATEX) ASLSyntaxReference.tex

ASLSemanticsReference.pdf: control ASLSyntaxReference.pdf ASLSemanticsReference.tex ASLSemanticsLines.tex
	$(LATEX) ASLSemanticsReference.tex
	$(LATEX) ASLSemanticsReference.tex
	$(LATEX) ASLSemanticsReference.tex

ASLTypingReference.pdf: control ASLSyntaxReference.pdf ASLTypingReference.tex ASLASTLines.tex ASLTypingLines.tex ASLTypeSatisfactionLines.tex
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

.PHONY: force

ifdef NOFORMAL
ifformal.tex: force
	@echo "\\\\newif\\ifformal\\\\formalfalse" >$@
else
ifformal.tex: force
	@echo "\\\\newif\\ifformal\\\\formaltrue" >$@
endif

ifdef NOEMPTY
ifempty.tex: force
	@echo "\\\\newif\\ifempty\\\\emptyfalse" >$@
else
ifempty.tex: force
	@echo "\\\\newif\\ifempty\\\\emptytrue" >$@
endif

ifdef TODO
iftodo.tex: force
	@echo "\\\\newif\\iftodo\\\\todotrue" >$@
else
iftodo.tex: force
	@echo "\\\\newif\\iftodo\\\\todofalse" >$@
endif

clean:
	/bin/rm -f $(PDFS)
	/bin/rm -f ASLSemanticsLines.tex ASLTypingLines.tex ASLTypeSatisfactionLines.tex ASLASTLines.tex
	/bin/rm -f *.aux *.log *.fls *.log *.toc *.fdb_latexmk *~
	/bin/rm -f $(CONTROLS)
	/bin/rm -f comment.cut
	/bin/rm -f *.blg *.bbl *.out
