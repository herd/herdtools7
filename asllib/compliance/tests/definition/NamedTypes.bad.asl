type Char of integer{0..255};
type Byte of integer{0..255};
constant K: Char = 210;
var b: Byte;

func f()
begin
    b = K; // Illegal: a Char cannot be directly assigned to a Byte
end;
