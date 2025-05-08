accessor X() <=> v: integer
begin
  getter
    return 0;
  end;

  setter
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
