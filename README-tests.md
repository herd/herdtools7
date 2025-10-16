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

By default. tests are compiled but do not run. To run the tests: `make
`...` RUN_TESTS=true`.
  + `make litmus-x86_64`, for x86_64.
  + `make litmus-aarch64`, for armv8.
