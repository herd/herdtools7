


.PHONY: asts

asts : aarch64haz-1-fragment.cat.lsp \
       aarch64haz-2-fragment.cat.lsp \
       aarch64haz-1.cat.lsp \
       aarch64haz-2.cat.lsp

%.cat.lsp : %.cat
	cat2lisp $*.cat > $*.cat.lsp
