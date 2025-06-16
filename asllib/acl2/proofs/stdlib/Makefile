

all : top.cert

stdlib-ast.lsp: empty.asl
	aslref --print-lisp --no-exec --no-primitives empty.asl > stdlib-ast.lsp

stdlib-prim-ast.lsp: empty.asl
	aslref --print-lisp --no-exec empty.asl > stdlib-prim-ast.lsp

asts: stdlib-prim-ast.lsp stdlib-ast.lsp

top.cert : stdlib-ast.lsp stdlib-prim-ast.lsp
	cert.pl top

.PHONY: clean all asts

clean:
	rm -f stdlib-ast.lsp stdlib-prim-ast.lsp
	clean.pl .
