func LimitBits{N}(x : bits(N), value : integer) => bits(N)
begin
    return value[N-1:0];
end

func Return32_bits{N}(data_in : bits(N)) => bits(32)
begin
    var data : bits(N) = data_in;
    return data[31:0];
end

// Create bit-vector of return type
func ReturnReturnType{M}(op : bits(M), N : integer) => bits(N)
begin
    var result : bits(N) = Ones(N);
    return result;
end

// Unused N
func UnusedUnderconstrained{N}(op1 : bits(N)) => bits(4)
begin
    return '1111';
end

// Going from unconstrained to constrained
func rolling0{N}(x : bits(N), shift : integer) => bits(N)
begin
    return rolling1(x, N-shift);
end

func rolling1{N}(x : bits(N), shift : integer) => bits(N)
begin
    let length = shift as integer{0..N};
    return Ones(length)[N-1:0];
end

// Condition on one underconstraint and generate a bit-vector based on return type
func ConditionalInput{N}(input : bits(N), esize : integer) => bits(esize)
begin
    if N == esize then
        return input[esize-1:0];
    end
    return Ones(esize);
end

func AmmendZero{N}(op : bits(N)) => bits(N)
begin
    return '0' :: op[N-2:0];
end

func NPlusM{N,M}(op1 : bits(M), op2 : bits(N)) => bits(M+N)
begin
    var result = Zeros(M+N);
    var result0 : bits(M+N) = ZeroExtend(op2, M+N);
    for i =0 to M-1 do
        result[i:] = '1';
    end

    return result;
end

func save_bits{N}(x : bits(N)) => bits(N)
begin
    var result : bits(N) = Ones(N);
    if N == 16 then
        var value = 100;
        return value[N-1:0];

    elsif N == 32 then
        result[31:0] = x[31:0];
        return result;
    end
    return result;
end
