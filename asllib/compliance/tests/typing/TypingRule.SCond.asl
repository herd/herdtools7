func conditionalStmt{N: integer {1,2}}(argT: bits(1), argF: bits(2)) => bits(N)
begin
    if (N == 1) then
        return argT as bits(N); // asserting type conversion is required
    else
        return argF as bits(N); // asserting type conversion is required
    end;
end;

func conditionalFun(N: integer {1,2}, argT: integer{1}, argF: integer{2}) => integer {1,2}
begin
    if (N == 1) then
        return argT;
    else
        return argF;
    end;
    // The return expression integer {1,2} satisfies the return type
    // No asserting type conversion is required.
end;