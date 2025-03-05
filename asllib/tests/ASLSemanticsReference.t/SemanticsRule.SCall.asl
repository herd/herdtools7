var g: bits(7);

func catenate_into_g{N, M}(x: bits(N), y: bits(M), order: boolean)
begin
    if order then
        g = (x :: y) as bits(7);
    else
        g = (y :: x) as bits(7);
    end;
end;

func zero() => integer
begin
  return 0;
end;

func main() => integer
begin
    var x = '1101';
    var y = Ones{3};
    assert g == Zeros{7};
    catenate_into_g{4, 3}(x, y, TRUE);
    assert g == '1101 111';

    // The following is illegal as 'zero' is not a procedure.
    // zero();
    return 0;
end;
