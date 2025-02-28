accessor X() <=> integer
begin
  getter begin
    return 0;
  end;

  setter = v begin
    pass;
  end;
end;

func main() => integer
begin
  assert X() == 0;
  let X = 1;
  X() = 2;

  return 0;
end;
