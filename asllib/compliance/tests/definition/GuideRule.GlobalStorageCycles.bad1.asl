// Illegal: invalid declaration of two variables with a cycle
// via the initialization expressions
var a = b;
var b = a;
