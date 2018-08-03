PREFIX=$$HOME

all:
	sh ./build.sh $(PREFIX)

install:
	sh ./install.sh $(PREFIX)

uninstall:
	sh ./uninstall.sh $(PREFIX)

clean:
	rm -f Version.ml
	ocamlbuild -clean
