// RUN: interp %s | FileCheck %s

func f() => integer {8,16,32,64}
begin
    return 8;
end

func g() => bits({8,16,32,64})
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
    var myVal: bits(myWid) = g();
    if myWid == 64 then
        myVal[31:0] = myFunc(myVal[31:0]);
        myVal[63:32] = myFunc(myVal[63:32]);
    else // author knows myVal is not bits(64)
        myVal = myFunc(myVal as bits({8,16,32}));
    end
end

func main() => integer
begin
    return 0;
end
