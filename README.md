This is herdtools7, a tool suite to test weak memory models.

We provide the following tools:

 - herd7: a generic simulator for weak memory models
 - litmus7: run litmus tests (given as assembler programs for
   Power, ARM, AArch64 or X86) to test the memory model of the
   executing machine
 - diy7: produce litmus tests from concise specifications
 - some additional tools
   In particular,
    * mcompare7 to analyse run logs of both herd and litmus.
    * klitmus7, an experimental tool, similar to litmus7 that runs kernel
      memory model tests as kernel modules. The tool klitmus7 is inspired
      from a python script by Andrea Parri,.
      <http://retis.sssup.it/~a.parri/lkmm/run.py>


herdtools7 is the successor of the diy tool suite.

Home
====

http://diy.inria.fr/

diy-devel@inria.fr

Compilation and installation
============================

See file [INSTALL.md](INSTALL.md).

License
=======

The authors of the diy7 tool suite are Jade Alglave and Luc Maranget.


Copyright 2010 -- present: Institut National de Recherche en Informatique et
en Automatique, and the authors.

Diy7 is released under the terms of the CeCILL-B free software license agreement.
See file [LICENSE.txt](LICENSE.txt).
