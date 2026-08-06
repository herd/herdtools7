Run litmus2desc on readers-guide tests in sorted order and compare output.

  $ descriptions=../../../readers-guide/litmus-descriptions
  $ tests=../../../readers-guide/litmus-tests
  $ for description in "$descriptions"/*.tex; do
  >   name=${description##*/}
  >   name=${name%.tex}
  >   HERDLIB=./libdir litmus2desc --latex --describe-dep-path \
  >     "$tests/$name.litmus" > generated.tex
  >   diff -u "$description" generated.tex
  > done
