accessor f1() <=> v: integer
begin
  getter
    return 4;
  end;

  setter
    Unreachable();
  end;
end;

func main () => integer
begin
  let x = f1;

  return 0;
end;
