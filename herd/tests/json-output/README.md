# herd7 JSON output

## Usage

With `herd7 -output-format json`, in addition to the normal output, the
following content are output as JSON:
- The litmus test itself (architecture, init, instructions, post conditions, etc.)
- The execution result (final states, verdict, etc.)
- Execution graphs as filtered by various `-show`, `-doshow`, and `-through` options.

With `-o <directory>`, each test produces a `.json` file. With `-o -`, JSON is
printed between `JSONBEGIN <name>` and `JSONEND <name>` lines in stdout, along
with the usual output.

The intended design is that `-output-format json` uses the latest schema
version, while `-output-format json.1` uses `schema-v1.json` and so on. This is
not implemented yet.

## Schema versions

Across the schema, optional fields may be absent or `null`.

- [Version 1 Schema](schema-v1.json)

## Implementation

The test and result JSON is implemented in [cli.ml](../../cli.ml). The execution
graph JSON is implemented in [Pretty.ml](../../Pretty.ml).

## Optional validation

An optional validation script is available as
[validate-schema.py](validate-schema.py). Since it assumes Python 3 and the
`jsonschema` package, it's an optional test, not in the default test suite.

If `jsonschema` is already available in your Python environment, run from the
repository root:

```sh
python3 herd/tests/json-output/validate-schema.py
```

For example, if you use `uv`, you can run in such an ad-hoc environment with:

```sh
uv run --no-project --with jsonschema python herd/tests/json-output/validate-schema.py
```

With no argument, the script discovers all `.litmus` files under this directory,
including symlinked fixtures, runs them, and validates their JSON output against
the schema.

You can also specify the schema version with `--schema-version N`, which will
use `schema-v{N}.json` as the schema file. By default, the latest version is
used (which is not auto discovered but needs to be updated in the script).

You can also override the `herd7` binary with `--herd /path/to/herd7`. By
default `dune build _build/default/herd/herd.exe` is executed once and the
resulting binary is used.

You can also pass in one positional argument to specify a litmus test. It will
be executed with a set of default arguments (see below for overriding them). The
JSON output arguments `-o - -output-format json` are always added.

```sh
python3 herd/tests/json-output/validate-schema.py --schema-version 1 \
  herd/tests/json-output/fixtures/self-modifying.litmus -- -variant self -show all
```

The one position argument of the litmus path, if present, must come before the
herd arguments. When the first argument starts with `"-"`, arguments not parsed
by this script are directly passed to `herd7`.

You can also use the explicit `--` separator. Everything after is passed to
`herd7`.

Check the script for default arguments. For fixtures under this directory, some
additional arguments are added by default when executed without the positional
litmus test.

The script exits with a nonzero status on invalid JSON, schema violations, or
execution errors.

