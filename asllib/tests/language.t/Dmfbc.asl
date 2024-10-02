//D_MFBC: A setter invocation consists of an assignment where:
//- part of the LHS consists of an identifier which denotes the name of
//the invoked setter, either on its own, or followed by a bracketed list
//of zero or more expressions which denote the actual arguments of the
//call.
//- the corresponding part of the RHS consists of an expression which
//denotes the actual argument corresponding to the setter’s RHS argument


// RUN: interp %s | FileCheck %s

getter test => integer
begin
    return 10;
end

setter test = value : integer
begin
    pass;
end

func main() => integer
begin
    test = 10;
    return 0;
end
