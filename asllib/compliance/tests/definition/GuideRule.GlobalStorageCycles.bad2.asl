// Illegal: an invalid declaration of 'var1' due to a cycle in its type specification
let size1 = Len(var1); // function Len() returns the width of var1
var var1 : bits(size1); // cycle -- the type of var1 depends on size1 which depends
                        // on var1
