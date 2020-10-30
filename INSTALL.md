Binary installation with OPAM
=======================

[Install OPAM](https://opam.ocaml.org/doc/Install.html), then:

    % opam install herdtools7

Then, to get the newest version:

    % opam update
    % opam upgrade

Source build
============

Tools will be installed in PREFIX/bin, and various files in PREFIX/share/herdtools7.
By default (see Makefile) PREFIX is $HOME.
You can change PREFIX by editing the Makefile, or by running ``make ...`` as ``make PREFIX=yourprefix ...``.

Requirements
------------

- OCaml (version >= 4.05.0)
- dune
- menhir (version >= 20180530)
- stdint library (version >= 0.5.1)


We strongly recommend to have this base software installed through the opam
package manager.

    % opam install dune menhir stdint

Notice: Compilation with ocamlbuild is possible, by setting D=ocb in Makefile.
For using ocamlbuild, supplementary software ocamlbuild and ocamlfind are required.
They can be installed with opam:

     % opam install ocamlfind ocamlbuild

Build
-----

    % make all

Install
-------

    % make install
