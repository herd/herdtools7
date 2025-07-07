pure func double (x: integer) => integer
begin
  return x * 2;
end;

constant A = double(32) as integer {0..1000};
constant C = double(double(16)) as integer {0..1000};
constant D: bits(A) = Zeros{C};

func main () => integer
begin
  return 0;
end;

