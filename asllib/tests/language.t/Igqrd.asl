// RUN: interp %s | FileCheck %s

config systemWid: integer {8,16} = 8;
// systemWid==>systemWid
func getDefault() => bits(systemWid)
begin
    var ret: bits(systemWid) = Zeros(systemWid);
    return ret;
end

func systemOp(x: bits(systemWid)) => bits(systemWid)
begin
    return (NOT x);
end

func getWidth {N: integer} (x: bits(N)) => integer
begin
    return N;
end

func test()
begin
    var addr = getDefault();
    // Type of addr is bits(systemWid)
    // which is a constrained width bitvector bits({8,16})
    addr = systemOp(addr); // Legal
    // Both LHS and RHS are bits(systemWid)
    let N = getWidth(addr) as {8,16};
    // Invokes execution-time check that RHS is IN {8,16}
    // Type of N is integer {8,16}
    // N==>N
    var newAddr: bits(N) = [ Zeros(N-1), '1'];
    // newAddr = systemOp(addr); // ILLEGAL
    // LHS is bits(N) but RHS is bits(systemWid)
    newAddr = systemOp(addr) as bits(N);
    // Incurs an execution-time check that (N==systemWid)
    // - = newAddr + addr; // ILLEGAL
    // Type checker cannot demonstrate (systemWid==N)
    newAddr = newAddr + (addr as bits(N)); // Legal
    // Incurs an execution-time check that (N==systemWid)
    // addr = newAddr; // ILLEGAL
    addr = newAddr as bits(systemWid); // Legal
end

func main() => integer
begin
    return 0;
end
