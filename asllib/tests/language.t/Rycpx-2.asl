//R_YCPX: An execution-time check for an asserted type conversion should not
//be failed before the expression is evaluated.

//For example, the check on y below should not cause an execution-time error
//if the invocation of f1 returns FALSE at execution time:

func checkY (y: integer)
begin
  if (f1() && f2(y as {2,4,8})) then pass; end
end
