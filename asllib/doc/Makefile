BENTO=../../_build/default/tools/bento.exe
LATEX=pdflatex

all: ASLSemanticsReference.pdf ASLTypingReference.pdf

ASLSemanticsReference.pdf: ASLSemanticsReference.tex ASLSemanticsLines.tex 
	$(LATEX) -pdf ASLSemanticsReference.tex

ASLTypingReference.pdf: ASLTypingReference.tex ASLASTLines.tex ASLTypingLines.tex ASLTypeSatisfactionLines.tex 
	$(LATEX) -pdf ASLTypingReference.tex

ASLSemanticsLines.tex: ../Interpreter.ml
	$(BENTO) $< > $@

ASLTypingLines.tex: ../Typing.ml
	$(BENTO) $< > $@

ASLTypeSatisfactionLines.tex: ../types.ml
	$(BENTO) $< > $@

ASLASTLines.tex: ../AST.mli
	$(BENTO) $< > $@

clean:
	/bin/rm -f ASLSemanticsReference.pdf ASLSemanticsLines.tex
	/bin/rm -f ASLTypingReference.pdf ASLTypingLines.tex ASLTypeSatisfactionLines.tex ASLASTLines.tex
	/bin/rm -f *.aux *.log *.fls *.log *.toc *.fdb_latexmk *~
