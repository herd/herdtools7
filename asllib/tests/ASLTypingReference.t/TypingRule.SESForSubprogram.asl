pure func foo() // GlobalEffect(SE_Pure), Immutability(TRUE)
begin
  pass;
end;

readonly func bar() // GlobalEffect(SE_Readonly), Immutability(FALSE)
begin
  pass;
end;

noreturn func goo() // GlobalEffect(SE_Impure), Immutability(FALSE)
begin
  pass;
end;

func baz() // GlobalEffect(SE_Impure), Immutability(FALSE)
begin
  pass;
end;
