c11popl15
=========

Translation of C11 memory model from "Common Compiler Optimisations are Invalid
in the C11 Memory Model and what we can do about it" (POPL '15)

Requirements
------------
   * python at least v2.7.3
   * herd from the github head, `https://github.com/herd/herdtools`, or
     herd from the internal UCL repository if you want the herd7 models
   * GNU Make

Generating test variants
------------------------

The paper describes a number of tests using some meta-constructs for ranging
over accesses.

  $ make variants

Running the regression
----------------------

The following runs a short regression (45 tests) by skipping the long running
fig6 test, only running the standard model over Appendix A and skipping all
appendix test variants:

  $ make quick_regression

Generating models
-----------------

Models are generated from the template `c11.cat-template` using `genmodel.py`.
Model options are given at the command-line. For example,

  $ ./genmodel.py --RF Naive --SC SCnew --RS RSorig --ST STnew

generates the model (Naive, SCnew, RSorig, STnew). By default, the script will
produce (ConsRFna, SCorig, RSorig, STorig). For all options use:

  $ ./genmodel.py --help

It is also possible to generate new herd7 models from the template
`c11.cat7-template`. Just pass the command `--herd7`:

  $ ./genmodel.py --herd7 --RF Naive --SC SCnew --RS RSorig --ST STnew

Generating and running models
-----------------------------

All commands passed after `--` are passed automatically through to herd (which
assumes that herd is in your path). For example:

  $ ./genmodel.py --RF Arfna -- -o tmp tests/illustrative/arfna.litmus

generates the model (Arfna, SCorig, RSorig, STorig) into `gen.cat` and then runs
the test arfna.litmus with the herd command:

  $ herd -conf c11.cfg -o tmp tests/illustrative/arfna.litmus

The configuration file `c11.cfg` picks up the generated model.

Generating all models and all tests to create a book
----------------------------------------------------

Use the `genbook.sh` script:

  $ ./genbook.sh

to generate all models and all tests into `c11popl15/c11/cats` and
`c11popl15/c11/tests`.

Any arguments passed to the script are passed automatically through to
`genmodel.py`. So the following may be useful:

  --strip-comments     # remove single-line comments from cat files
  --inline-libraries   # remove library dependencies by inlining
  --herd7              # generate using herd7 model
