type shape of integer;
type square of integer subtypes shape;
var myShape: shape;
var mySquare: square;

// following declarations of f are illegal since
// both have the structure of an integer so their argument type-clashes
func f(x: shape)
begin
    pass;
end;

func f(y: square)
begin
    pass;
end;

func f_test()
begin
    // if the declarations were legal, the which f would be invoked?
    f(mySquare);
end;
