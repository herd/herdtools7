type time of integer;
func f(r: time) begin pass; end;
// Illegal as `time` type-clashes with `integer`.
func f(r: integer) begin pass; end;
