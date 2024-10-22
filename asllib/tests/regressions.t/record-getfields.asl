type R of record {
  a: integer,
  b: bits(3),
  c: bits(4),
};

func main () => integer
begin 
  var x = R {
    a = 32,
    b = Zeros(3),
    c = '1010'
  };

  let y = x.(b, c);
  assert y == '0001010';

  x.(c, b) = '1100111';

  let z = x.(b, c);
  assert z == '1111100';

  return 0;
end

