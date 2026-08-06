## Building the document

### Building the pdf

We assume LaTeX is installed on the system.

To build `ReadersGuide.pdf`, run:

```sh
make
```

This will:

- Run `pdflatex`, `bibtex`, then `pdflatex` twice more to generate `ReadersGuide.pdf`
- Stop on LaTeX errors and check the LaTeX log for missing-file diagnostics

This command can be run every time a source file changes.

### Assets

The document uses a few checked-in asset directories:

- `litmus-tests/` contains the litmus tests printed in the guide.
- `litmus-descriptions/` contains generated LaTeX prose descriptions for selected litmus tests.
- `figures/` contains the PDF figures included by `ReadersGuide.tex`.

The description files are automatically generated from `.litmus` sources using
the `litmus2desc` tool. They are kept checked into version control so that the
Readers Guide can be built without having an OCaml toolchain installed. They
also serve as authoritative snapshots for the `litmus2desc` test suite.

## Refreshing generated litmus descriptions

To regenerate the prose description files `litmus-descriptions/*.tex`, run:

```sh
opam exec -- dune build --root .. tools/litmus2desc/bin/main.exe  # Build litmus2desc
make descriptions                                                 # Generate descriptions
```

Note that this step is _not_ needed for normal builds of the Reader's Guide PDF.
It is only needed to refresh litmus test descriptions after changing a
corresponding litmus test, the cat model, or `litmus2desc` itself.

By default, `litmus2desc` uses the development cat model in `herd/libdir`.
Override its location with `HERDLIB`:

```sh
make descriptions HERDLIB=/path/to/herd/libdir
```
