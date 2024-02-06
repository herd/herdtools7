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

Contributing
============

This repository uses the [Pre-Commit tool](https://pre-commit.com) to manage
pre-commit validation, to check for formatting, test regressions, etc.

Pre-Commit can be installed on macOS with [Homebrew](https://brew.sh), or on
all platforms with Python's `pip`:

    # macOS with Homebrew.
    % brew install pre-commit

    # All other OS (including macOS without Homebrew).
    % pip install pre-commit

To make Pre-Commit run automatically when you `git commit`, add it to your Git
repository's local `pre-commit` hooks. From within this repository, run:

    % pre-commit install

When adding a new pre-commit check, please run Pre-Commit manually first:

    % pre-commit run --all-files

### Pull Request Methodology
We would like to, as much as possible, maintain a linear git history, have pull
requests reviewed, and finally merged responsibly. Therefore, we ask all
contributors to follow the following procedure:
1. Create a git branch: 'git checkout -b feature_name'
2. Implement new feature/change.
3. Commit with meaningful messages:
   'git commit -m "[feature] one line title followed by detailed description"'
4. Test the feature and make sure all relevant tests pass, including
   'make test'.
5. Push 'git push'
6. Rebase often, repeating 2--6 as much as needed:
  - 'git checkout master'
  - 'git pull'
  - 'git checkout feature_name'
  - 'git rebase'
  - Resolve all conflicts.
  - 'git rebase --continue'
  - 'git push --force'
7. Create a pull request and ask for reviews.
8. Address reviewer comments and make sure all issues are resolved.
9. Once the pull request is accepted, let **the repo admin** merge.

License
=======

The authors of the diy7 tool suite are Jade Alglave and Luc Maranget.


Copyright 2010 -- present: Institut National de Recherche en Informatique et
en Automatique, and the authors.

Diy7 is released under the terms of the CeCILL-B free software license agreement.
See file [LICENSE.txt](LICENSE.txt).
