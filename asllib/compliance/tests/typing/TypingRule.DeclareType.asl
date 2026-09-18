type Color of enumeration {RED, GREEN, BLUE};

type TimeType of integer;

let num_bits = 16;
type Record of record { data: bits(num_bits) };

constant num_exception_bits = 32;
type Exception of exception { data: bits(num_exception_bits) };

config num_exception_collection : integer{16, 32} = 32;
var Collection : collection { data: bits(num_exception_collection) };
