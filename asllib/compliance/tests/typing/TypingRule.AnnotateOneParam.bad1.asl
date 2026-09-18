var A: integer;

// Illegal: `A` is also declared as a global storage element.
func parameterized{A}(x: bits(A)) begin pass; end;
