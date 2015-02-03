** What is diy? **

This is diy version a tool suite to test weak memory models.

We provide the following subtools
 - herd: A generic simulator for weak memory models.

 - litmus: Given assembler programs for Power, ARM or X86 (a litmus test)
           generates C programs, whose execution
           will test the memory model of the executing machine.
           
 - diy proper: Produce litmus tests from concise specifications.

 - Some tools to analyse run logs of both herd and litmus.
 

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

Herd authors are Jade Alglave and Luc Maranget.

Litmus authors are Luc Maranget and Susmit Sarkar.

Diy proper authors are Jade Alglave and Luc Maranget.

Copyright 2010--2013: Institut National de Recherche en Informatique et
en Automatique, and the authors.

Diy is released under the terms of the Gnu Lesser General Public License
(LGPL). See file LICENSE.txt

