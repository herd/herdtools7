all:	gen.ml gen_parser.mly gen_lexer.mll gen_ast.ml
	ocamlyacc gen_parser.mly
	ocamllex gen_lexer.mll
	ocamlopt -o gen gen_ast.ml gen_parser.mli gen_parser.ml gen_lexer.ml gen.ml 
