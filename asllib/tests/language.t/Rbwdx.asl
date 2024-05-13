//R_BWDX: Fields of an exception can be read or written independently using
//the syntax r.f to refer to field f of an exception r.

// RUN: interp %s | FileCheck %s

type a of exception{
    b: integer
};

type BAD_OPCODE of exception{};
type UNDEFINED_OPCODE of exception {reason: string, opcode: bits(16)};

func test()
begin
    throw UNDEFINED_OPCODE{reason="Undefined", opcode='0111011101110111'};
end

func main() => integer
begin
    var aa : a;

    aa.b = 10;

    var bb : integer = aa.b;

    print(bb);
    return 0;
end
