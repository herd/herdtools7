Kinds come from the condition; verdicts use the same rules as text output.

  $ report () { herd7 -set-libdir ../../libdir -show none -o - -output-format json "$@" | grep -E '"(kind|verdict)":'; }
  $ report fixtures/static-poi-pagealign.litmus
      "kind": "Allowed",
      "verdict": "Ok",
  $ sed 's/0:X0=2/0:X0=3/' fixtures/static-poi-pagealign.litmus > absent.litmus
  $ report absent.litmus
      "kind": "Allowed",
      "verdict": "No",
  $ sed 's/exists/~exists/' fixtures/static-poi-pagealign.litmus > forbidden.litmus
  $ report forbidden.litmus
      "kind": "Forbidden",
      "verdict": "No",
  $ sed 's/exists/~exists/' absent.litmus > forbidden-absent.litmus
  $ report forbidden-absent.litmus
      "kind": "Forbidden",
      "verdict": "Ok",
  $ report fixtures/C14.litmus
      "kind": "Required",
      "verdict": "Ok",
  $ sed 's/y=2/y=3/' fixtures/C14.litmus > required-absent.litmus
  $ report required-absent.litmus
      "kind": "Required",
      "verdict": "No",

Undefined executions take precedence over the condition verdict.

  $ sed 's/__int128/int/g' fixtures/C11.litmus > undefined.litmus
  $ report -badexecs true undefined.litmus
      "kind": "Allowed",
      "verdict": "Undef",
