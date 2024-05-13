//R_XZVT: The type of a conditional expression is the lowest common ancestor
//of the types of the then and else expressions.

func conditionalFun(N: integer {1,2}, argT: integer{1}, argF: integer{2}) => integer {1,2}
begin
  if (N==1) then
    return argT;
  else
    return argF;
  end
// The return expression integer {1,2} satisfies the return type
// No checked type conversion is required.
end
