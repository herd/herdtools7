//I_CJVD: The items (name or dash) in the parenthesized list on the
//left-hand side are positionally paired with the elements from the tuple on
//the right-hand side. For each pair, an assignment is made from the
//right-hand side element to the left-hand side element.

// RUN: interp %s | FileCheck %s

func main() => integer
begin
    var (a, b) : (boolean, integer) = (TRUE, 1);

    return 0;
end
