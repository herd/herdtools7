With no `-` or positional arguments, stdin is interpreted as a list of filenames.

  $ echo 'R.litmus' | mprog7 -mode text | head -1
  X86_64 R

In a stdin list, a line containing "-" names the literal file `./-`.

  $ echo '-' | mprog7 -mode text | head -1
  X86_64 Dash

A failed file does not prevent later files from being processed, but makes the
final exit status non-zero.

  $ printf '%s\n' R.litmus missing.litmus - | mprog7 -mode text > batch.out 2> batch.err
  [1]
  $ grep X86_64 batch.out
  X86_64 R
  X86_64 Dash
  $ cat batch.err
  Fatal error: File "missing.litmus" open_in failed: missing.litmus: No such file or directory

A literal dash can also be passed explicitly as "./-".

  $ mprog7 -mode text ./- | head -1
  X86_64 Dash

Similarly, a "-" line in a @list file is interpreted as the literal file `./-`.

  $ mprog7 -mode text @inputs | grep X86_64
  X86_64 R
  X86_64 Dash

An unreadable @list aborts with a non-zero exit status.

  $ mprog7 @missing 2> /dev/null
  [2]
