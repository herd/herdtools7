//D_VXKM: A procedure call consists of an identifier which denotes the
//name of the called procedure, followed by a parenthesised list of zero
//or more expressions which denote the actual arguments of the call. 

// RUN: interp %s | FileCheck %s

func test()
begin
    pass;
end

func main() => integer
begin
    test();
    return 0;
end
