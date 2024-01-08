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

comment:
	$(MAKE) $(MFLAGS) COMMENT=1 all

CONTROL=ifempty.tex ifformal.tex control.tex ifcomment.tex

ASLSyntaxReference.pdf: $(CONTROL) ASLASTLines.tex ASLSyntaxReference.tex
	$(LATEX) -pdf ASLSyntaxReference.tex
	$(BIBTEX) ASLSyntaxReference
	$(LATEX) -pdf ASLSyntaxReference.tex
	$(LATEX) -pdf ASLSyntaxReference.tex

ASLSemanticsReference.pdf: $(CONTROL) ASLSyntaxReference.pdf ASLSemanticsReference.tex ASLSemanticsLines.tex
	$(LATEX) -pdf ASLSemanticsReference.tex
	$(BIBTEX) ASLSemanticsReference
	$(LATEX) -pdf ASLSemanticsReference.tex
	$(LATEX) -pdf ASLSemanticsReference.tex

ASLTypingReference.pdf: $(CONTROL) ASLSyntaxReference.pdf ASLTypingReference.tex ASLASTLines.tex ASLTypingLines.tex ASLTypeSatisfactionLines.tex
	$(LATEX) -pdf ASLTypingReference.tex
	$(BIBTEX) ASLTypingReference
	$(LATEX) -pdf ASLTypingReference.tex
	$(LATEX) -pdf ASLTypingReference.tex

ASLRefProgress.pdf: ASLRefProgress.tex
	$(LATEX) -pdf $<

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

ifdef COMMENT
ifcomment.tex: force
	@echo "\\\\newif\\ifcomment\\\\commenttrue" >$@
else
ifcomment.tex: force
	@echo "\\\\newif\\ifcomment\\\\commentfalse" >$@
endif

clean:
	/bin/rm -f $(PDFS)
	/bin/rm -f ASLSemanticsLines.tex ASLTypingLines.tex ASLTypeSatisfactionLines.tex ASLASTLines.tex
	/bin/rm -f *.aux *.log *.fls *.log *.toc *.fdb_latexmk *~
	/bin/rm -f ifformal.tex ifempty.tex ifcomment.tex
	/bin/rm -f comment.cut
	/bin/rm -f *.blg *.bbl *.out
