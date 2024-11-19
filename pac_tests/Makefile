NAME = example4

all: my_herd
	touch build/example.svg
	dot -Tsvg build/$(NAME).dot > build/example.svg

my_herd:
	../_build/install/default/bin/herd7 \
		-show prop \
		-conf graph.cfg -gv $(NAME).litmus -o build \
		-set-libdir ../herd/libdir \
		-cat aarch64.cat

herd:
	herd7 \
		-show prop -doshow prop \
		-conf graph.cfg -gv $(NAME).litmus -o build \
		-cat aarch64.cat
