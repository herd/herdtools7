//R_XZVT: The type of a conditional expression is the lowest common ancestor
//of the types of the then and else expressions.

func conditionalFun(N: integer {1,2}, argT: bits(1), argF: bits(2)) => bits(N)
begin
  if (N==1) then
    return argT as bits(N);
  else
    return argF as bits(N);
  end 
end
