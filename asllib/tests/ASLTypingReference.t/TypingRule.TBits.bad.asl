func foo(I: integer)
begin
    var R: bits(I); // illegal: I is unconstrained.
end;
