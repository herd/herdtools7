func bad_elide_empty_argument_list()
begin
  let x : bits(64) = Foo{}; // tries to construct empty record `Foo`
end;

func main() => integer
begin
  return 0;
end;
