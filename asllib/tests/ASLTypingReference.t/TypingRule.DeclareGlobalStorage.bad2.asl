let x = 5;
// Illegal: expressions in type annotation must be constant-time.
config c : integer{1..x} = 2;
