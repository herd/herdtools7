Line terminators are recognized as LF or CRLF. Each form terminates line comments
and advances the source position once, including inside multi-line comments.

  $ aslref line-terminators-lf.asl
  File line-terminators-lf.asl, line 6, characters 9 to 16:
    return missing;
           ^^^^^^^
  ASL Static error (TE_UI): Undefined identifier: 'missing'
  [1]

  $ printf 'func main () => integer\r\nbegin\r\n  /* first\r\n     second */\r\n  // comment\r\n  return missing;\r\nend;\r\n' > crlf.asl
  $ aslref crlf.asl
  File crlf.asl, line 6, characters 9 to 16:
    return missing;
           ^^^^^^^
  ASL Static error (TE_UI): Undefined identifier: 'missing'
  [1]

Bare carriage returns do not match a lexical element in ordinary source, line
comments, or multi-line comments.

  $ printf 'let x = 0;\r' > bare-cr.asl
  $ aslref bare-cr.asl
  File bare-cr.asl, line 1, characters 10 to 11:
  let x = 0;
            ^
  ASL Lexical error (BE_LE): Unknown symbol (ASCII code point(s): 13).
  [1]

  $ printf '// comment\r' > bare-cr-line-comment.asl
  $ aslref bare-cr-line-comment.asl
  File bare-cr-line-comment.asl, line 1, characters 10 to 11:
  // comment
            ^
  ASL Lexical error (BE_LE): Unknown symbol (ASCII code point(s): 13).
  [1]

  $ printf '/* comment\r' > bare-cr-multiline-comment.asl
  $ aslref bare-cr-multiline-comment.asl
  File bare-cr-multiline-comment.asl, line 1, characters 10 to 11:
  /* comment
            ^
  ASL Lexical error (BE_LE): Unknown symbol (ASCII code point(s): 13).
  [1]
