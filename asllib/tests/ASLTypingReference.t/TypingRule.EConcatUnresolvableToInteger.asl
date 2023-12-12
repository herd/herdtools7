func foo{N}(x: bits(N)) => bit
begin
    return x[0];
end

config LIMIT1: integer = 2;
config LIMIT2: integer{1, 2, 3, 4, 5, 6, 7, 8, 9, 10} = 7;

func bar() => integer{1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
begin
    var ret: integer = 0;
    while ret < LIMIT1 do
        ret = ret + ret * 2;
    end
    return ret as integer{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
end

func main() => integer
begin
    let N = bar();
    let M = LIMIT2;
    let x = Zeros(N);
    let y = Zeros(M);
    let z = foo([x, y]);
    return 0;
end
