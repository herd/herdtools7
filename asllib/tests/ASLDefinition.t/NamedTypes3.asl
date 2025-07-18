// Neither ADDR nor PHYSICAL_ADDR is a subtype of the other.
type ADDR of bits (32) {};
type PHYSICAL_ADDR of ADDR;
var addr: ADDR;
var physical: PHYSICAL_ADDR;

// For the function "raw_addr",
func raw_addr(x: ADDR) => bits(32)
begin
    // x may be used as the expression in the return statement
    // since the return type is type satisfied by the type of x
    return x;
end;

func raw_physical_addr(x: PHYSICAL_ADDR) => bits(32)
begin
    return x;
end;

func addresses()
begin
    var tmp: bits(32);
    // primitive type bits(32) is type-satisfied by both ADDR and PHYSICAL_ADDR
    tmp = addr;
    physical = tmp;
    tmp = '0' :: tmp[30:0];
    addr = tmp;
    physical = raw_addr(addr);
    addr = raw_physical_addr(physical);
    physical = addr[31:0]; // a bitslice is of type bits(N)
    addr = physical[31:0];
end;
