BENTO=../../_build/default/tools/bento.exe

all: ASLSemanticsReference.pdf ASLTypingReference.pdf

ASLSemanticsReference.pdf: ASLSemanticsReference.tex ASLSemanticsLines.tex 
	pdflatex -pdf ASLSemanticsReference.tex

ASLTypingReference.pdf: ASLTypingReference.tex ASLTypingLines.tex 
	pdflatex -pdf ASLTypingReference.tex

ASLSemanticsLines.tex: ../Interpreter.ml
	$(BENTO) $< > $@

ASLTypingLines.tex: ../Typing.ml
	$(BENTO) $< > $@

clean:
	/bin/rm -f ASLSemanticsReference.pdf ASLSemanticsLines.tex
	/bin/rm -f ASLTypingReference.pdf ASLTypingLines.tex
	/bin/rm -f *.aux *.log *.fls *.log *.toc *.fdb_latexmk *~
