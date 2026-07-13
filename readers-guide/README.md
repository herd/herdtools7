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

### Refreshing generated litmus descriptions

We assume the tool `litmus2desc` is installed.

To regenerate the prose description files `litmus-descriptions/*.tex`, run:

```sh
make descriptions
```

This will re-generate descriptions for a subset of the litmus tests stored in
`litmus-tests/`. Note that litmus description files `litmus-descriptions/*.tex`
are already under version control, so this `make descriptions` step is
necessary _only if_ the contents of `litmus-tests/` has changed.

If `litmus2desc` lives elsewhere, pass its location explicitly. If the herd
library directory needs to be set, use the standard `HERDLIB` environment
variable understood by herdtools:

```sh
HERDLIB=/path/to/herd make descriptions LITMUS2DESC=/path/to/litmus2desc
```
