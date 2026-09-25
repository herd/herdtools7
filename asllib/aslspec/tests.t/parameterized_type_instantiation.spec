typedef Int;

typedef Tree[[T]] =
  | Leaf(value: T)
;

typing relation make_leaf(x: Int) -> (Tree[[Int]])
{
  prose_transition = ""
} =
  --
  Leaf(x);
;

typedef Box[[T]] =
  | BoxRecord[field: T]
;

typing relation make_box(x: Int) -> (Box[[Int]])
{
  prose_transition = ""
} =
  --
  BoxRecord[field: x];
;

typing relation read_box(box: Box[[Int]]) -> Int
{
  prose_transition = ""
} =
  --
  box.field;
;

typing relation update_box(box: Box[[Int]], x: Int) -> (Box[[Int]])
{
  prose_transition = ""
} =
  --
  box(field: x);
;

typedef RawBox[[T]] =
  [raw_field: T]
;

typing relation make_raw_box(x: Int) -> (RawBox[[Int]])
{
  prose_transition = ""
} =
  --
  [raw_field: x];
;

typing relation read_raw_box(box: RawBox[[Int]]) -> Int
{
  prose_transition = ""
} =
  --
  box.raw_field;
;

typing relation update_raw_box(box: RawBox[[Int]], x: Int) -> (RawBox[[Int]])
{
  prose_transition = ""
} =
  --
  box(raw_field: x);
;

typedef ListAlias[[T]] =
  list0(T)
;

typing relation index_list(xs: ListAlias[[Int]], i: N) -> Int
{
  prose_transition = ""
} =
  --
  xs[i];
;

typedef Endofunction[[T]] =
  fun T -> T
;

typedef FunctionHolder[[T]] =
  | FunctionHolderValue[fn: Endofunction[[T]]]
;

typing relation apply_function(holder: FunctionHolder[[Int]], x: Int) -> Int
{
  prose_transition = ""
} =
  --
  holder.fn(x);
;

typedef Pair[[T]] =
  | PairValue(left: T, right: T)
;

typing relation make_pair(x: Int, y: Int) -> (Pair[[Int]])
{
  prose_transition = ""
} =
  --
  PairValue(x, y);
;
