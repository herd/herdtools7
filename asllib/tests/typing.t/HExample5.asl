func halfsize0(op1 : bits(N DIV 2), op2 : bits(N DIV 2), N : integer) => bits(N)
begin
    var value1 : bits(N) = op1 :: Zeros(N DIV 2);
    return value1;
end

func halfsize1{N}(op : bits(N)) => bits(N DIV 2)
begin
    var result : bits(N);
    let halfsize = N DIV 2;
    return result[(2*halfsize)-1:halfsize];
end

func halfsize2{N}(op1 : bits(N) , op2 : bits(N)) => bits(N)
begin
    var result : bits(2*N);
    return result[2*N-1:N];
end

func halfsize3(size : integer) => bits(size*8)
begin
    var value = UNKNOWN: bits(size*8);
    let halfsize = (size DIV 2) as integer{4,8};
    var lowhalf : bits(halfsize * 8);
    var highhalf : bits(halfsize * 8);
    lowhalf = returnOnes(halfsize);
    highhalf = returnOnes(halfsize);
    value = highhalf :: lowhalf;

    return value;
end

func returnOnes(size : integer) => bits(8*size)
begin
    return Ones(8*size);
end
