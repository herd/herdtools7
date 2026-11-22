typedef Item
  @prose_description "item"
  @math_macro \Item
  @math_layout _
  @short_circuit_macro \SomeItem
= ItemNode @prose_description "item node";

constant without_layout = ItemNode;
constant with_layout = ItemNode @math_layout _;
constant nested_layout = (ItemNode, (ItemNode, ItemNode))
  @math_layout [_, [_, _] ];

variadic operator join[T](values: list1(T)) -> T
  @associative
  @custom
  @math_macro \joinop
  @prose_application "joining {values}";

operator cast[T](value: T) -> T
  @typecast
  @prose_application "{value}";

relation identity(x: Item) -> (y: Item)
  @prose_description "maps {x} to {y}"
  @prose_application "the identity of {x}"
  @prose_transition "mapping {x} yields"
  @math_layout ((_), _)
=
  --
  x @math_layout _;
;

render default_items = Item(-);
render repeated_items @no_lhs_hypertargets = Item(-);
