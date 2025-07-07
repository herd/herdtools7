accessor f1() <=> v: integer
begin
  readonly getter
    return 4;
  end;

  setter
    unreachable;
  end;
end;

func main () => integer
begin
  let x = f1;

  return 0;
end;
