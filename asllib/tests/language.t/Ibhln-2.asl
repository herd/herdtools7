//I_BHLN: In the following example, the actual argument of the invocation of
//myFunc on the last line has an asserted type conversion which does not
//supply a width. This results in the actual argument being treated as a
//bitvector of determined width equal to the determined width of myVal, but
//with a different constraint from that of myVal, namely {8,16,32} instead
//of {8,16,32,64}.
//The type checker knows that the invocation of myFunc returns a value whose
//width is the same as the width of the actual argument. However, the
//constraint {8,16,32,64} on myVal alone would not satisfy the constraint
//8,16,32 on the formal argument myInput hence the checked type conversion
//is required.

// RUN: interp %s | FileCheck %s

func f() => integer {8,16,32,64}
begin
    return 8;
end

func g() => bits(8)
begin
    return Zeros(8);
end

func myFunc {N: integer{8,16,32}} (myInput: bits(N)) => bits(N)
begin
    return Zeros(N);
end

func MyVectorInstruction()
begin
    let myWid: integer {8,16,32,64} = f();
    var myVal: bits(myWid) = g() as bits(myWid);
    if myWid == 64 then
        myVal[31:0] = myFunc(myVal[31:0]);
        myVal[63:32] = myFunc(myVal[63:32]);
    else // author knows myVal is not bits(64)
        myVal = myFunc(myVal as bits(8)) as bits(myWid);
    end
end

func main() => integer
begin
    return 0;
end
