Reading a test from stdin produces the same output as reading it from a file.

  $ mprog7 -mode text A.litmus > from-file.txt
  $ mprog7 -mode text - < A.litmus > from-stdin.txt
  $ diff -u from-file.txt from-stdin.txt

Standard input can only be selected once.

  $ mprog7 - - 2> /dev/null
  [2]

When combined with `-o`, a litmus test passed via stdin is saved with name
`stdin.litmus`.

  $ mkdir text-out
  $ mprog7 -mode text -o text-out - < A.litmus
  $ ls text-out
  stdin.litmus
  $ diff -u from-stdin.txt text-out/stdin.litmus
