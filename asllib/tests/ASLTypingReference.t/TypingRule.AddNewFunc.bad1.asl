type shape of integer;
type square of integer;
var myShape: shape;
var mySquare: square;

// following declarations of f are illegal since
// both have the structure of an integer so their arguments type-clash
func f(x: shape)
begin
    pass;
end;

func f(y: square)
begin
    pass;
end;
