BENTO=../../_build/default/tools/bento.exe

all: ASLSemanticsReference.pdf

ASLSemanticsReference.pdf: ASLSemanticsReference.tex ASLSemanticsLines.tex 
	latexmk -pdf ASLSemanticsReference.tex

ASLSemanticsLines.tex: ../Interpreter.ml
	$(BENTO) $< > $@

clean:
	/bin/rm -f ASLSemanticsReference.pdf ASLSemanticsLines.tex
	/bin/rm -f *.aux *.log *.fls *.log *.toc *.fdb_latexmk *~
