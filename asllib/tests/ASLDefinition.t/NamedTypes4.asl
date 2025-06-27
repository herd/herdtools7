type Char of integer{0..255};
type Byte of integer{0..255};
constant K: Char = 210;
var c: Char;
var b: Byte;

func f()
begin
    c = 210; // legal: c has the structure of integer and can be assigned an integer
    c = K; // legal: K has type Char and can be assigned to a Char
end;
