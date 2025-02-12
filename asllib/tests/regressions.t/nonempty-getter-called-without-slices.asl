accessor f1() <=> integer
begin
  getter begin
    return 4;
  end;

  setter = v begin
    Unreachable();
  end;
end;

func main () => integer
begin
  let x = f1;

  return 0;
end;
