let x = 5;
// Illegal: initializing expression must be constant-time.
config c : integer{1..5} = x;
