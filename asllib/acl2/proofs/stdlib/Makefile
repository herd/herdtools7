

all : top.cert

stdlib-ast.lsp: empty.asl
	aslref --print-lisp --no-exec --no-primitives empty.asl > stdlib-ast.lsp

top.cert : stdlib-ast.lsp
	cert.pl top

.PHONY: clean

clean:
	rm -f stdlib-ast.lsp
	clean.pl .
