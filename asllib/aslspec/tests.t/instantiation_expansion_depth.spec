typedef A =
    | Leaf
    | Node(A)
;

typedef B =
    | ( Node(Node(Leaf)) )
;
