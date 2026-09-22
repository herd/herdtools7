var MyCollection: collection { a: bits(8), b: bits(16) };
var CollectionWithoutFields: collection{-};

// The next declaration in comment is illegal:
// only bitvector types are allowed as collection fields.
// var IllegalCollection: collection { non_bitvector: integer };

// The next two declarations in comments are illegal:
// a global storage element of collection
// type must supply a type annotation and no initialization expression.
// var x of collection { a: bits(8), b: bits(16) } = MyCollection {a = Zeros{8}, b = Zeros{16}};
// var - : collection { a: bits(8), b: bits(16) } = MyCollection {a = Zeros{8}, b = Zeros{16}};

func main() => integer
begin
    // The next declaration in comment is illegal:
    // local storage elements of collection types are forbidden.
    // var - : collection { a: bits(8), b: bits(16) };
    return 0;
end;
