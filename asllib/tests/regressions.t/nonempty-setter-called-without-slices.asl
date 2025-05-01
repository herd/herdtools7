accessor f1() <=> v: integer
begin
  getter
    return 4;
  end;

  setter
    pass;
  end;
end;

func main () => integer
begin
  f1 = 4;

  return 0;
end;
