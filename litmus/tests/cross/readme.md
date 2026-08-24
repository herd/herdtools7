# Cross-architecture tests

These tests are meant to be architecture-independent, but they are intended to
run under Linux only.
This is due to the availability of QEMU's user mode, which enables the
architecture independence in the first place.

# Use cases
1. User adds new test case
2. User wants to run tests while developing
3. Tests are run when opening a PR, the code has been merged upstream, and when there's a release

# Design
The tests are defined as cram tests, which allows parallelization at the single test level, and helps reduce the amount of ad-hoc code by using dune's infrastructure.

This means the tests can be run in a development loop, in a more targeted way by running `dune build @litmus/tests/cross/AArch64/runtest-litmus-cross-aarch64 -w`.
The tests are organized by the architecture they test, and they are defined individually in a way that is easy to add new ones.
This should be possible by duplicating an existing test, changing the name of the test, and running `dune build @runtest-litmus-cross-<arch> --autopromote`

Cross-compiler packaging varies from distribution to distribution, here are some of the packages for distribution, and the command name, if it differs from the package:

AArch64 cross-compiler packages:
- debian, ubuntu: gcc-aarch64-linux-gnu (aarch64-linux-gnu-gcc)
- rhel (epel), fedora: gcc-aarch64-linux-gnu (aarch64-linux-gnu-gcc)
- alpine: gcc-aarch64-none-elf (aarch64-none-elf-gcc)

x86_64 cross-compiler pacakges
- debian, ubuntu: gcc-x86-64-linux-gnu (x86_64-linux-gnu-gcc)
- rhel (epel), fedora: gcc-x86_64-linux-gnu (x86_64-linux-gnu-gcc)
- alpine: gcc-x86_64 (x86_64-elf-gcc)

Thankfully, the big two packaging traditions agree in the name of both the package and commands, so cram tests use these names.

On top of that the packages are available for all the supported architectures, so no gating for them should be needed.
