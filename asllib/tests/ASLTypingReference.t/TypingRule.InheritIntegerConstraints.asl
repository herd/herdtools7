constant max_bits = 64;
var b : integer{1..5, 7, 20..max_bits};
var c : integer{6..9};

func main() => integer
begin
    var e : integer{-} = b + c;
    var f : integer{-} = 5;
    return 0;
end;
