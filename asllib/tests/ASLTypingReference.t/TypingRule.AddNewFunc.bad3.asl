type shape of integer;
type square of integer subtypes shape;
var myShape: shape;
var mySquare: square;

// following declarations of h are illegal
// since all arguments type-clash with corresponding arguments in the other declaration
func h(x: shape, y: square)
begin
    pass;
end;

func h(x: square, y: shape) // Illegal
begin
    pass;
end;

func h_test()
begin
    // if the declarations were legal, the which h would be invoked?
    h(mySquare, mySquare); // either h may apply!!
end;
