var state : integer = 0;
var called_override = FALSE;

impdef accessor Foo() <=> integer
begin
  getter begin
    return state;
  end;

  setter = v begin
    state = v;
  end;
end;

implementation accessor Foo() <=> integer
begin
  getter begin
    called_override = TRUE;
    return state;
  end;

  setter = v begin
    state = 42;
  end;
end;

func main() => integer
begin
  let - = Foo();
  assert called_override;
  Foo() = 1;
  assert state == 42;
  return 0;
end;
