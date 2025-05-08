impdef func Foo{N: integer{32,64}}(n : boolean) => bits(N)
begin
  return Zeros{N+1};
end;

implementation func Foo{N: integer{32,64}}(n : boolean) => bits(N)
begin
  return Ones{N};
end;
