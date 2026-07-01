type Word1 of integer{0..31};
type Exc of exception {i: integer};

func main() => integer
begin
    var arr1 : array[[8]] of Word1;
    var ex : Exc;
    // The following statement in comment is illegal as 'array[[8]] of Word1'
    // and Exc do not have lowest common ancestor.
    var o  = if ARBITRARY : boolean then arr1 else ex;
    return 0;
end;
