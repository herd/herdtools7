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
We would like, as much as possible, to maintain a linear git history,
since it is simple, readable, and easier to bisect in case of bugs.
We would also like to have pull requests reviewed, and finally merged
responsibly.
Therefore, our recommendation to all contributors is to follow the following
procedure:
1.  Create a [fork](https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/working-with-forks/fork-a-repo)
    of `herdtools7` named, for example, `upstream` and set `herdtools7` as your remote.
2.  Create a git branch: `git checkout -b feature`
3.  Implement new feature/change.
4.  Commit with meaningful messages:
    `git commit -m "[tag] one line descriptive title, followed by detailed description"`
    The tag(s) should refer to the component that the feature relates to, e.g.,
    `[herd]`, `[litmus]`, etc. Generally, the tags correlate to the sub-folders.
    Also, `[all]` applies to changes that effect several components.
5.  Test the feature and make sure all relevant tests pass, including
    `make test`.
6.  Assuming `origin` is your clone of `herdtools7`, push with
    `git push --set-upstream origin feature`
7.  Rebase often, repeating 2--6 as much as needed:
   - `git fetch upstream`
   - `git rebase -i upstream/master`
   - If there are conflicts, resolve all of them and perform a `git rebase --continue`
   - `git push --force`
8.  Create a pull request and ask for reviews.
9.  Address reviewer comments and make sure all issues are resolved.
19. Once the pull request is accepted, let **a repo admin**, a.k.a the merger,
    perform the merge.
    The merger has to fill the merge message, and it is often convenient to copy
    the initial message of the conversation, which usually describes the effect
    and purpose of the PR.

License
=======

The authors of the diy7 tool suite are Jade Alglave and Luc Maranget.


Copyright 2010 -- present: Institut National de Recherche en Informatique et
en Automatique, and the authors.

Diy7 is released under the terms of the CeCILL-B free software license agreement.
See file [LICENSE.txt](LICENSE.txt).
