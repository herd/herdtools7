func bad_tuple() => (integer{42}, integer{0})
begin
  let y : (integer{}, boolean, integer{}) = (42, TRUE, 43);
  return (y.item0, y.item2);
end;

func main() => integer
begin
  return 0;
end;
