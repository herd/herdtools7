# ASL compliance tests

This directory contains the ASL compliance test suite.

The suite is intended to provide a structured, reviewable collection of ASL
programs and their expected results. Each test consists of an ASL source file and
a sibling YAML metadata file with the same stem:

```text
example.asl
example.yaml
```

The YAML metadata follows `schema.yaml`. The metadata records whether the test
is expected to succeed or fail, the execution mode, expected output when
applicable, and ASL Reference error information for failing tests.

## Directory layout

```text
asllib/compliance/
  schema.yaml          Metadata schema for compliance test YAML files.
  tests/               Compliance test sources and expected YAML metadata.
  asltest.ml           Test executable used by generated Dune rules.
  gen.ml               Generates Dune rules for all test/source pairs.
  validate-schemas.py  Validates test YAML files against schema.yaml.
```

The `tests/` directory may contain nested subdirectories. Dune rules are
generated dynamically for every `.asl` / `.yaml` pair discovered under `tests/`.

## Running the compliance tests

From the repository root:

```sh
dune runtest asllib/compliance
```

The generated rules run ASLRef through `asltest`, produce an actual YAML result
for each test, and compare it with the expected YAML file in `tests/`.

To update expected YAML files after an intentional change:

```sh
dune runtest asllib/compliance --auto-promote
```

or run the tests and then promote the generated changes:

```sh
dune promote
```

Review promoted YAML changes carefully. They are the observable expected results
of the compliance suite.

Promotion rewrites expected YAML files using the formatting produced by
`asltest`. This keeps metadata files consistent, including key ordering and
block formatting, but may replace hand-written YAML formatting.

## Adding a test

Add an `.asl` file under `tests/` and a sibling `.yaml` file with the same stem.
For a new test, the YAML file can start with just the execution mode:

```yaml
mode: exec
```

or:

```yaml
mode: no-exec
```

Then run:

```sh
dune runtest asllib/compliance --auto-promote
```

This fills in the expected result produced by ASLRef. Check the promoted YAML
before committing it.

## Validating metadata schemas

The YAML files can be validated directly with:

```sh
python3 asllib/compliance/validate-schemas.py
```

This requires Python packages for YAML and JSON Schema support. On Ubuntu:

```sh
sudo apt-get install --yes python3-yaml python3-jsonschema
```

The CI test workflow installs these packages and runs the validator before the
main test suite.
