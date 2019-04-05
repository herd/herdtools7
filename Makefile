PREFIX=$$HOME
D=dune-

all: $(D)build

install: $(D)install

uninstall:
	sh ./uninstall.sh $(PREFIX)

clean: $(D)clean
	rm -f Version.ml

ocb-build:
	sh ./build.sh $(PREFIX)

ocb-install:
	sh ./install.sh $(PREFIX)

ocb-clean:
	ocamlbuild -clean

dune-build:
	sh ./dune-build.sh $(PREFIX)

dune-install:
	sh ./dune-install.sh $(PREFIX)

dune-clean:
	dune clean
