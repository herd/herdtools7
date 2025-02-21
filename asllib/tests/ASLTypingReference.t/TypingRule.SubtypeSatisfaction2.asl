// Declare some named types
type superInt of integer;
type subInt of integer subtypes superInt ;
type uniqueInt of superInt;

func assign()
begin
    // Integer is subtype-satisfied by all the named types,
    // so it can be assigned to them by the assignment and
    // initialization type checking rules
    var myInt: integer;
    var mySuperInt : superInt = myInt;
    var mySubInt : subInt = myInt;
    var myUniqueInt: uniqueInt = myInt;
    // Integer is subtype-satisfied by all the named types,
    // so it can be assigned from them by the assignment and
    // initialization type checking rules
    myInt = mySuperInt;
    myInt = mySubInt;
    myInt = myUniqueInt;

    // superInt is not a subtype of anything (apart from itself)
    // so it cannot be assigned to any other named type
    // Illegal: mySubInt = mySuperInt;
    // Illegal: myUniqueInt = mySuperInt;
    // subInt is a subtype of superInt, so the assignment and
    // initialization type checking rules permit the following:
    mySuperInt = mySubInt;
    // But subInt and uniqueInt are not subtype related
    // so do not type-satisfy each other.
    // Illegal: myUniqueInt = mySubInt;
    // uniqueInt has no related subtype or supertype
    // so it cannot be assigned to any named type
    // Illegal: mySuperInt = myUniqueInt;
    // Illegal: mySubInt = myUniqueInt;
end;
