func foo(I: integer)
begin
    var R: bits(I); // Illegal since I is unconstrained.
end;
