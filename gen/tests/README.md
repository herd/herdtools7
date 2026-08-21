# Individual `diyone7` tests

The following directory contains individual tests for `diyone7`:

- `AArch64`

Each file starts with the `diyone7` command that must reproduce the remainder
of the file:

```text
(* diyone7 -arch AArch64 -metadata false -oneloc PosRW Rfi *)
AArch64 CoRW1+pos-rfi
...
```
For successful commands, the filename before `.litmus` must exactly match the
generated `AArch64 <name>` line. Rejected commands use a descriptive
`reject-...` filename. `-metadata false` keeps the output independent of
version metadata.

`internal/diyone_test.ml` recursively finds files with this directive. `show`
prints the commands, `test` compares generated and recorded output after
trimming whitespace at the beginning and end, and `promote` rewrites the body
and prints the directive in canonical form.

Standard error is recorded before standard output inside a comment so that
`herd7` ignores it. A non-zero exit status is recorded in the same comment as
`[n]`.

### Edge coverage

We test individual edges and annotations.
Old compatibility aliases are not tested separately. Each edge is placed in a
minimal cycle according to its source and target directions:

| Direction | Command shape |
|-----------|---------------|
| `RR` | `PodWW Rfe <edge> Fre` |
| `RW` | `<edge> Rfi` |
| `WR` | `<edge> Fri` |
| `WW` | `<edge> Coi` |

The `RW`, `WR`, and `WW` commands use `-oneloc`. The `RR` cycle uses different
locations and covers `PodRR`, explicit `dRR` fences, and `Dp*dR` dependencies.
Same-location `sRR` cases are not included.

Insertion edges are tested after an explicit `Po` edge. For example,
`DMB.ST` is covered by:

```text
PosRW DMB.ST Rfi
PosWR DMB.ST Fri
PosWW DMB.ST Coi
PodWW Rfe PodRR DMB.ST Fre
```

### Annotation coverage

Annotations are tested with the following strategies. Plain `P`
annotations are omitted and represented by the unannotated individual-edge
tests.

| Coverage | Edges | Command pattern |
|----------|-------|-----------------|
| Read annotation | `DpCtrlsW`, `DpDatasW`, `DpAddrsW`, `PosRW`, `LxSx`, `Amo.Cas`, `Amo.Swp` | `<read_annotation> <edge> Rfi` |
| Write annotation | `DpCtrlsW`, `DpDatasW`, `DpAddrsW`, `PosRW`, `LxSx`, `Amo.Cas`, `Amo.Swp` | `<edge> <write_annotation> Rfi` |
| Two read annotations | `PosRW` | `<read_annotation> <read_annotation> PosRW Rfi` |
| Two write annotations | `PosRW` | `PosRW <write_annotation> <write_annotation> Rfi` |
| One read and one write annotation | `Amo.Cas`, `Amo.Swp` | `<read_annotation> <edge> <write_annotation> Rfi` |
| One annotation on each read | N/A | `PodWW Rfe <read_annotation> PodRR <read_annotation> Fre` |

Exhaustive ordered-pair exploration shows that consecutive annotations are
accepted only when they are identical and valid for that endpoint. Different
annotations, represented by `Pa A` on reads and `Pa L` on writes, are tested as
rejected combinations with `PosRW`. `Amo.Cas` and `Amo.Swp` test at most one
annotation on the read and one annotation on the write. Other invalid single
annotations are also retained as negative tests.
