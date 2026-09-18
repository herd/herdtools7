func f{N: integer {2,4}}(arg0: bits(N), arg1: bits(N))
begin return; end;

func test{wid: integer {2,4,8}}(bus: bits(wid))
begin
    f{wid}(bus, bus); // Illegal
    // wid does not type-satisfy the invocation type of N which is integer{2,4}
end;
