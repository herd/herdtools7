NAME = example1

all: #my_herd
	touch build/example.svg
	dot -Tsvg build/$(NAME).dot > build/example.svg

my_herd:
	../_build/install/default/bin/herd7 \
		-show prop -doshow prop \
		-conf graph.cfg -gv $(NAME).litmus -o build \
		-set-libdir ../herd/libdir \
		-showevents all -show all

herd:
	herd7 \
		-show prop -doshow prop -variant mte \
		-conf graph.cfg -gv $(NAME).litmus -o build \
		-cat aarch64.cat -showevents all -show all

litmus:
	../_build/install/default/bin/litmus7 \
		-os mac -set-libdir ../litmus/libdir \
		$(NAME).litmus


litmus_kvm:
	../_build/install/default/bin/litmus7 \
		-set-libdir ../litmus/libdir \
		$(NAME).litmus \
		-mach kvm-m1 -variant fatal \
		-o build/$(NAME).tar
	#mkdir -p build/$(NAME)
	#tar xmf build/$(NAME).tar -C build/$(NAME)
	mkdir -p kvm-unit-tests/$(NAME)
	tar xmf build/$(NAME).tar -C kvm-unit-tests/$(NAME)

clean:
	rm -rf build/*

