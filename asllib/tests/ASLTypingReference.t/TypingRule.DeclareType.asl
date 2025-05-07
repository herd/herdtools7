type Color of enumeration {RED, GREEN, BLUE};

type SubColor subtypes Color;
// The following type declaration in comment is illegal:
// enumeration types do not declare fields.
// type SubColorWithFields subtypes Color with { status: boolean };

type TimeType of integer;

let num_bits = 16;
type Record of record { data: bits(num_bits) };
type SubRecordEmptyExtraFields subtypes Record with {-};
type SubRecordNoExtraFields subtypes Record;
type SubRecord subtypes Record with { status: boolean };

constant num_exception_bits = 32;
type Exception of exception { data: bits(num_exception_bits) };
type SubException subtypes Exception with { status: boolean };

config num_exception_collection : integer{16, 32} = 32;
type Collection of collection { data: bits(num_exception_collection) };

// The following type declaration in comment is illegal:
// collection types cannot be subtyped.
// type SubCollection subtypes Collection with { status: boolean };
