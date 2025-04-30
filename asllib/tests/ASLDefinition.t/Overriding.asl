impdef func Foo{N: integer{32,64}}(
  n : boolean,
  mask : bits(64) { [0] lsb }) => bits(N)
begin
  return Zeros{N};
end;

impdef func Bar(n : integer) => integer
begin
  return n;
end;

implementation func Foo{N: integer{32,64}}(
  n : boolean,
  mask : bits(64) { [0] lsb }) => bits(N)
begin
  return Ones{N};
end;

func main() => integer
begin
  let res = Foo{32}(TRUE, Ones{64});
  return 0;
end;
