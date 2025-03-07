func main () => integer
begin
    // Illegal: integers can only be compared to integers.
     assert 3 IN { <= 42.0 };
    // Illegal: reals can only be compared to reals.
    assert 3 IN { <= 42.0 };
    var x : integer;
    // Illegal: expressions must be symbolically evaluable.
    assert 42.0 IN { <= x };
  return 0;
end;
