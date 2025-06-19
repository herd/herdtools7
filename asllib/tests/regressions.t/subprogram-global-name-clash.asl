var X : integer = 0;

accessor X() <=> v: integer
begin
  readonly getter
    return X;
  end;

  setter
    X = v;
  end;
end;

var Y : integer = 0;

readonly func Y(b: boolean) => integer
begin
  return Y;
end;

func main() => integer
begin
  - = X;
  - = Y(TRUE);
  X() = X;
  X() = X + 1;
  assert X() == 1;

  return 0;
end;
