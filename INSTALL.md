With OPAM (recommended)
=======================

[Install OPAM](https://opam.ocaml.org/doc/Install.html), then:

    % opam install herdtools7

Then, to get the newest version:

    % opam update
    % opam upgrade

Without OPAM
============

Tools will be installed in PREFIX/bin, and various files in PREFIX/share/herdtools7.
By default (see Makefile) PREFIX is $HOME.
You can change PREFIX by editing the Makefile, or by running ``make ...`` as ``make PREFIX=yourprefix ...``.

Requirements
------------

- OCaml (version >= 4.05.0)
- dune*

* Compilation with ocamlbuild is possible, by setting D=ocb in Makefile.

Build
-----

    % make all

Install
-------

    % make install
