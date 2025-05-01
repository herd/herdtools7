var X : integer = 0;

accessor X() <=> v: integer
begin
  getter
    return X;
  end;

  setter
    X = v;
  end;
end;

func X(b: boolean) => integer
begin
  return X;
end;

func main() => integer
begin
  - = X;
  - = X(TRUE);
  X() = X;
  X() = X + 1;
  assert X() == 1;

  return 0;
end;
