func main() => integer
begin
    var bv : bits(8);
    assert UInt(bv) == UInt{8}(bv);
    assert ZeroExtend{16, 8}(bv) == Zeros{16};
    assert ZeroExtend{16}(bv) == Zeros{16};
    let - : bits(16) = ZeroExtend{}(bv);
    let res: bits(64) = Zeros{}();
    return 0;
end;
