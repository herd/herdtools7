accessor f1() <=> integer
begin
  getter begin
    return 4;
  end;

  setter = v begin
    pass;
  end;
end;

func main () => integer
begin
  f1 = 4;

  return 0;
end;
