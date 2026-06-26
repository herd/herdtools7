type circle of integer;
type square of integer;
var myCircle: circle;
var mySquare: square;

// following declarations of h are illegal
// since all arguments type-clash with corresponding arguments in the other declaration
func h(x: circle, y: square)
begin
    pass;
end;

func h(x: square, y: circle) // Illegal
begin
    pass;
end;
