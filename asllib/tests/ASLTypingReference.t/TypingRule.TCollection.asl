type MyCollection of collection { a: bits(8), b: bits(16) };
type CollectionWithEmptyFieldList of collection {-};
type CollectionWithoutFields of collection;

// The next type declaration in comment is illegal:
// only bitvector types are allowed as collection fields.
// type IllegalCollection of collection { non_bitvector: integer };

// The next two declarations in comments are illegal:
// a global storage element of collection
// type must supply a type annotation and no initialization expression.
// var - = MyCollection {a = Zeros{8}, b = Zeros{16}};
// var - : MyCollection = MyCollection {a = Zeros{8}, b = Zeros{16}};

var x : MyCollection;
var y : CollectionWithEmptyFieldList;
var z :CollectionWithoutFields;

func main() => integer
begin
    // The next declaration in comment is illegal:
    // local storage elements of collection types are forbidden.
    // var - : MyCollection;
    return 0;
end;
