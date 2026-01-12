

all : top.cert

# Note: we load empty.asl in ASLv0 mode so that aslref will produce both v0 and v1 versions of certain functions
stdlib-ast.lsp: empty.asl
	aslref --print-lisp --no-exec --no-primitives -0 empty.asl > stdlib-ast.lsp

stdlib-prim-ast.lsp: empty.asl
	aslref --print-lisp --no-exec -0 empty.asl > stdlib-prim-ast.lsp

asts: stdlib-prim-ast.lsp stdlib-ast.lsp

top.cert : stdlib-ast.lsp stdlib-prim-ast.lsp
	cert.pl top

.PHONY: clean all asts

clean:
	rm -f stdlib-ast.lsp stdlib-prim-ast.lsp
	clean.pl .
