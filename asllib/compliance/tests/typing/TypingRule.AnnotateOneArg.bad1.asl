var b : boolean;

// Illegal: `b` is also declared as a global storage element.
func arguments(b: boolean, i: integer, r: real)
begin
    - = b;
    - = i;
    - = r;
end;
