// RUN: interp %s | FileCheck %s

func invokeMe {N: integer {8,16,32}} (x: bits(N))
begin
    return;
end

func test(M: integer {8,16,32}, L: integer {8,16})
begin
    var myM: bits(M);
    var myL: bits(L);
    if (M != L) then
        return;
    end
    // Note the type-checker does not do full program analysis
    // So it does not know that M==L after this statement
    // myM = myL; // ILLEGAL
    // myM and myL are constrained width bitvectors of determined widths
    // M and L respectively.
    // The type-checker does not know (M==L), so subtype-satisfaction
    // disallows this use of myL.
    myM = myL as bits(M); // Legal
    // The author explicitly claimed that myL has the width of myM
    // An execution-time check of (M==L) is required
    invokeMe(myL); // Legal
    // The parameter N is taken to be the value which corresponds
    // with the width of myL and the width of myL is an integer {8,16}
    // which complies with the declaration of parameter 'N'
    // The rules for subtype-satisfaction are satisfied since
    // the formal 'x' and the actual 'myL' are of the same determined width.
end

func main() => integer
begin
    return 0;
end
