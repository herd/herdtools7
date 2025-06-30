type MyException of exception{-};

func f() => integer
begin
    throw MyException{-};
end;

// Initializing a global integer variable
// results in a thrown exception.
var x = f();

// main will no be evaluated because of the failure to evaluate `x`.
func main() => integer
begin
    return 0;
end;
