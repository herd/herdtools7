Parsing a malformed model must result in exit status 2.

  $ herd7 -model aarch64.cat minimal.litmus
  herd7: File "./aarch64.cat", line 2, character 0: Lex error eof in skip_comment (in model)
  [2]

  $ herd7 -set-libdir . minimal.litmus
  Warning: File "./aarch64.cat", line 2, character 0: Lex error eof in skip_comment (in model) (User error)
  [2]
