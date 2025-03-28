func X() => integer begin return 0; end;

func X() // Illegal: `X` is also declared as a function.
begin pass; end;
