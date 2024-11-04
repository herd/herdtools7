func foo(x: integer {0..10}, y: integer {0..10})
begin
  let z = Zeros{64};
  print(z[x:y]);
end;

func main() => integer
begin
    foo(4, 2);
    foo(2, 4);
    return 0;
end;
