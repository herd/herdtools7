//R_PNQJ: A declaration using a parenthesized list of identifiers (or nested
//parenthesized lists) requires an initializer expression with the structure
//of a tuple. The type for each of the identifiers may be omitted and
//instead the type for each local storage element will be the type of the
//positionally paired element (or nested tuple) of the initializer tuple
//expression.

// RUN: interp %s | FileCheck %s

let a = (10, 10);

func main() => integer
begin
    return 0;
end
