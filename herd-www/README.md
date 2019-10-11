This is herd-www a web interface for the herd memory model simulator

This web interface is part of the herdtools7 tool suite.

Home
====

A live demo of herd-www is available at http://diy.inria.fr/www/

Contact: diy-devel@inria.fr

Compilation and installation
============================

Dependencies.
- ocaml, ocamlbuild, js_of_ocaml, js_of_ocaml-ppx.

Install all those with opam!
```
% opam install ocamlbuild js_of_ocaml js_of_ocaml-ppx
```

Build
-----
```
% make
```
Will install a working system in sub-directory www. It remains to
copy www to its final destination, e.g.

```
% rsync -av www $HOME/public_html/diy
```

Credits
======
The initial authors of herd-www are  Jules Villard and Jade Alglave
The authors of the herdtools7 tool suite are Jade Alglave and Luc Maranget.

License
=======

Copyright 2010 -- present: Institut National de Recherche en Informatique et
en Automatique, and the authors.

herdtools7 is released under the terms of the CeCILL-B free software license agreement.

See file [LICENSE.txt](../LICENSE.txt).

herd-www uses the (Bootstrap web framework)[https://www.w3schools.com/bootstrap/default.asp], including some glyphs from the (Glyphicon Halflings set)[https://glyphicons.com/].