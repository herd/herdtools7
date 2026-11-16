The loop needs three iterations to reach the condition. With a smaller unrolling
limit, JSON must report the same Loop verdict as stdout and preserve the warning
on stderr.

  $ herd7 -set-libdir ./libdir -unroll 1 -show none -o - -output-format json fixtures/unrolling-limit.litmus > truncated.out 2> truncated.err
  $ cat truncated.err
  Warning: File "fixtures/unrolling-limit.litmus": unrolling limit exceeded at loop, legal outcomes may be missing.
  $ grep -E '"(verdict|observation|cutoff)":' truncated.out
      "verdict": "Loop No",
      "observation": "Never",

The warning is identical to the existing text-mode warning.

  $ herd7 -set-libdir ./libdir -unroll 1 -show none fixtures/unrolling-limit.litmus > text.out 2> text.err
  $ diff -u text.err truncated.err
  $ grep '^Loop ' text.out
  Loop No

With enough iterations, the outcome is found, the verdict has no Loop prefix,
and there is no warning.

  $ herd7 -set-libdir ./libdir -unroll 3 -show none -o - -output-format json fixtures/unrolling-limit.litmus > complete.out 2> complete.err
  $ cat complete.err
  $ grep -E '"(verdict|observation|cutoff)":' complete.out
      "verdict": "Ok",
      "observation": "Always",
