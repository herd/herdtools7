func main() => integer
begin
  var bv = ARBITRARY: bits(64);
  bv[(2)*:(4)] = Zeros{4};

  return 0;
end;
