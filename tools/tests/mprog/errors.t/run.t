Fatal errors and user errors have distinct diagnostic labels.

  $ mprog7 missing.litmus
  Fatal error: File "missing.litmus" open_in failed: missing.litmus: No such file or directory
  [1]

  $ mprog7 - < AArch64-invalid.litmus
  User error: File "stdin.litmus", line 4, characters 3-5: unexpected 'L0' (in prog)
  [1]
