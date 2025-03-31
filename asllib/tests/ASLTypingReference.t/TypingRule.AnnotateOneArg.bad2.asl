type MyCollection of collection;

// Illegal: collection types are not allowed as arguments.
func arguments(b: MyCollection)
begin pass; end;
