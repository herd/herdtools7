Single impdef only
  $ aslref --no-exec --overriding-permissive impdef-only.asl
  $ aslref --no-exec --overriding-all-impdefs-overridden impdef-only.asl
  File impdef-only.asl, line 1, character 0 to line 4, character 4:
  impdef func Foo{N: integer{32,64}}(n : boolean) => bits(N)
  begin
    return Zeros{N};
  end;
  ASL Warning: Missing `implementation` for `impdef` function.
  $ aslref --no-exec --overriding-no-implementations impdef-only.asl

Single impdef overridden by single implementation
  $ aslref --overriding-permissive impdef-overridden.asl
  $ aslref --no-exec --overriding-all-impdefs-overridden impdef-overridden.asl
  $ aslref --no-exec --overriding-no-implementations impdef-overridden.asl
  File impdef-overridden.asl, line 6, character 0 to line 9, character 4:
  implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N)
  begin
    return Ones{N};
  end;
  ASL Warning: Unexpected `implementation` function.

Implementation without impdef
  $ aslref --no-exec --overriding-permissive implementation-only.asl
  File implementation-only.asl, line 1, character 0 to line 4, character 4:
  implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N)
  begin
    return Zeros{N};
  end;
  ASL Typing error: no `impdef` for `implementation` function.
  [1]
  $ aslref --no-exec --overriding-all-impdefs-overridden implementation-only.asl
  File implementation-only.asl, line 1, character 0 to line 4, character 4:
  implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N)
  begin
    return Zeros{N};
  end;
  ASL Typing error: no `impdef` for `implementation` function.
  [1]
  $ aslref --no-exec --overriding-no-implementations implementation-only.asl
  File implementation-only.asl, line 1, character 0 to line 4, character 4:
  implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N)
  begin
    return Zeros{N};
  end;
  ASL Warning: Unexpected `implementation` function.

Clashing implementations
  $ aslref --no-exec --overriding-permissive clashing-implementations.asl
  File clashing-implementations.asl, line 1, character 0 to line 4, character 4:
  implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N)
  begin
    return Zeros{N};
  end;
  ASL Typing error: multiple overlapping `implementation` functions for Foo:
    File clashing-implementations.asl, line 1, character 0 to line 4,
      character 4
    File clashing-implementations.asl, line 6, character 0 to line 9,
      character 4
  [1]
  $ aslref --no-exec --overriding-all-impdefs-overridden clashing-implementations.asl
  File clashing-implementations.asl, line 1, character 0 to line 4, character 4:
  implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N)
  begin
    return Zeros{N};
  end;
  ASL Typing error: multiple overlapping `implementation` functions for Foo:
    File clashing-implementations.asl, line 1, character 0 to line 4,
      character 4
    File clashing-implementations.asl, line 6, character 0 to line 9,
      character 4
  [1]
  $ aslref --no-exec --overriding-no-implementations clashing-implementations.asl
  File clashing-implementations.asl, line 1, character 0 to line 4, character 4:
  implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N)
  begin
    return Zeros{N};
  end;
  ASL Typing error: multiple overlapping `implementation` functions for Foo:
    File clashing-implementations.asl, line 1, character 0 to line 4,
      character 4
    File clashing-implementations.asl, line 6, character 0 to line 9,
      character 4
  [1]

Clashing impdefs
  $ aslref --no-exec --overriding-permissive clashing-impdefs.asl
  File clashing-impdefs.asl, line 11, character 0 to line 14, character 4:
  implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N)
  begin
    return Zeros{N};
  end;
  ASL Typing error: multiple `impdef` candidates for `implementation`:
    File clashing-impdefs.asl, line 1, character 0 to line 4, character 4
    File clashing-impdefs.asl, line 6, character 0 to line 9, character 4
  [1]
  $ aslref --no-exec --overriding-permissive clashing-impdefs-without-implementation.asl
  File clashing-impdefs-without-implementation.asl, line 6, character 0 to
    line 9, character 4:
  impdef func Foo{N: integer{32,64}}(n : boolean) => bits(N)
  begin
    return Zeros{N};
  end;
  ASL Typing error: cannot declare already declared element "Foo".
  [1]

Bad implementations
  $ aslref --no-exec --overriding-permissive bad-implementation-name.asl
  File bad-implementation-name.asl, line 6, character 0 to line 9, character 4:
  implementation func Bar{N: integer{32,64}}(n : boolean) => bits(N)
  begin
    return Ones{N};
  end;
  ASL Typing error: no `impdef` for `implementation` function.
  [1]
  $ aslref --no-exec --overriding-permissive bad-implementation-param.asl
  File bad-implementation-param.asl, line 6, character 0 to line 9, character 4:
  implementation func Foo{N: integer{64}}(n : boolean) => bits(N)
  begin
    return Ones{N};
  end;
  ASL Typing error: no `impdef` for `implementation` function.
  [1]
  $ aslref --no-exec --overriding-permissive bad-implementation-arg.asl
  File bad-implementation-arg.asl, line 6, character 0 to line 9, character 4:
  implementation func Foo{N: integer{32,64}}(m : boolean) => bits(N)
  begin
    return Ones{N};
  end;
  ASL Typing error: no `impdef` for `implementation` function.
  [1]
  $ aslref --no-exec --overriding-permissive bad-implementation-return.asl
  File bad-implementation-return.asl, line 6, character 0 to line 9,
    character 4:
  implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N+1)
  begin
    return Ones{N};
  end;
  ASL Typing error: no `impdef` for `implementation` function.
  [1]

Interactions with other features
  $ aslref --overriding-permissive overriding-overloading.asl
  $ aslref --overriding-permissive overriding-accessors.asl
  $ aslref --overriding-permissive --no-exec type-check-impdef.asl
  File type-check-impdef.asl, line 3, characters 2 to 20:
    return Zeros{N+1};
    ^^^^^^^^^^^^^^^^^^
  ASL Typing error: a subtype of bits(N) was expected, provided bits((N + 1)).
  [1]
