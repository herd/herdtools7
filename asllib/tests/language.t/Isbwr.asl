//I_SBWR: When determining whether a formal bitvector argument’s declared
//type is type-satisfied by its invocation type, parameter values are known,
//so the declared type, the invocation type and the actual’s type will all
//be bitvectors of determined width, hence subtype-satisfaction will not
//compare their domains.


// RUN: interp %s | FileCheck %s

func test(a: bits({0..3}))
begin
    pass;
end

func main() => integer
begin
    var a: bits({1..3}) = '11';
    test(a);
    return 0;
end
