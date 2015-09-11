** What is diy7 ? **

This is diy7, a tool suite to test weak memory models.

We provide the following subtools
 - herd7: A generic simulator for weak memory models.

 - litmus7: Given assembler programs for Power, ARM or X86 (a litmus test)
           generates C programs, whose execution
           will test the memory model of the executing machine.
           
 - diy7 proper: Produce litmus tests from concise specifications.

 - Some tools to analyse run logs of both herd and litmus.
 
** diy7 is the successor of the diy tool suite.

** Home **
http://diy.inria.fr/
diy-devel@inria.fr


** Compilation and installation **
See file INSTALL.txt


** Contents of this release **
   litmus/  Litmus sources
   gen/     Diy proper sources
   doc/     Documentation
   example/ Example

** Law **

The authors of the diy7 tool suite are Jade Alglave and Luc Maranget.

Copyright 2010--present: Institut National de Recherche en Informatique et
en Automatique, and the authors.

Diy7 is released under the terms of the CeCILL-B free software license
agreement. See file LICENSE.txt

