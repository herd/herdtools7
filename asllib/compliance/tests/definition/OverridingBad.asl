implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N)
begin
  return Zeros{N};
end;

implementation func Bar(n : integer) => integer
begin
  return n;
end;

implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N)
begin
  return Ones{N};
end;
