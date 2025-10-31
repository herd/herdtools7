# Makefile entries for tests

## Herd testing
  + `make test`, "basic" test suite, peformed by CI for every push in PR's
  + `make test-local` subset of `make test` that should run without an
    Internet connection.
  + `make cata-test` tests from the catalogue that run in decent time,
    included in `make test`.
  + `make cata-test-asl`, all ASL catalogue tests.
  + `make test-all`,"complete" test suite, run every week by the CI.
     Does *not* include ASL vmsa tests, of which running time is high.
  + `make test-all-asl`, all ASL tests, includes *all* ASL tests.
     Does include  ASL vmsa tests.

## Litmus testing

The basic litmus test consists in compiling series of tests. Some series are also executed but not all of them. Tests that are not run by default would take too much time or raise errors.

  + `make litmus-x86_64-test`, for x86_64. All tests are executed by default.
  + `make litmus-aarch64-test`, for armv8. Some tests are executed some other are not.
      - `make litmus-aarch64-run` to compile and execute the test series that are executed by default.
      - `make litmus-aarch64-norun` to compile the test series that are not executed by default

Notice:
  1. One can use the command line to force execution (`make ... RUN_TESTS=true`) or to prevent execution (`make ... RUN_TESTS=false`).
  2. Litmus testing must be performed on machines whose processor matches the architecture tested.
