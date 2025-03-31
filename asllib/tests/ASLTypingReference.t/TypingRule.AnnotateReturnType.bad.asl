type MyCollection of collection;

// Illegal: collection types are not allowed as return types.
func returns_value() => MyCollection
begin
    return ARBITRARY: MyCollection;
end;
