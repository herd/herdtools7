
// Illegal: collection types are not allowed as return types.
func returns_value() => collection { foo: bits(32)};
begin
    return ARBITRARY: collection { foo: bits(32)};
end;
