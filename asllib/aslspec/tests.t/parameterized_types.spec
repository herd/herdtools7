typedef Tree[[T]] =
    | Leaf(value: T)
    | Node(left: Tree[[T]], right: Tree[[T]])
;