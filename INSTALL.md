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

- OCaml (version >= 4.08.0)
- dune
- menhir (version >= 20180530)
- zarith

We strongly recommend to have this base software installed through the opam
package manager.

    % opam install dune menhir zarith

Make sure to run `eval $(opam config env)` to make tools available in your PATH.

Notice: Compilation with ocamlbuild is not longer possible

Build
-----

    % make all

Testing
-------

The optionnal dependency `qcheck` can be installed with `opam` as follows:

    % opam install qcheck

Whether or not optional dependencies are installed, the following command
runs the tests, skipping the ones that necessitate non-available dependencies.

    % make test

Install
-------

    % make install
