#! /bin/sh
find . -name '*.mli' -o -name '*.ml'\
 -o -name '*.mll' -o -name '*.mly'\
 -o -name '*.c' -o -name '*.h' |\
 grep -v _build | \
 grep -v version.ml | \
 grep -v legacy


