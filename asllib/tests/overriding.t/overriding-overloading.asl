
impdef func Foo() => integer
begin
  return 0;
end;

implementation func Foo() => integer
begin
  return 1;
end;

impdef func Foo(n: integer) => integer
begin
  return n;
end;

implementation func Foo(n: integer) => integer
begin
  return n + 1;
end;

impdef func Foo(n: bits(4)) => bit
begin
  return n[0];
end;

func main() => integer
begin
  assert (Foo() == 1);
  assert (Foo(1) == 2);
  assert (Foo('0001') == '1');

  return 0;
end;
