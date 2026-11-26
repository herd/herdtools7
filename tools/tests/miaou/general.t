General relation inlining checks.

Inlining produces the same description as writing the expanded relation,
including optional sequences, closures, and identity restrictions.

  $ check_inline () { miaou7 -q -set-libdir ../libdir -show bar "$@" inline.cat > actual && miaou7 -q -set-libdir ../libdir -show bar "$@" explicit.cat > expected && diff -u expected actual; }
  $ cat > inline.cat <<'EOF'
  > let foo = po
  > let bar = foo;rf?
  > EOF
  $ cat > explicit.cat <<'EOF'
  > let bar = po;rf?
  > EOF
  $ check_inline -inline foo

  $ cat > inline.cat <<'EOF'
  > let foo = rf
  > let bar = po;foo?
  > EOF
  $ check_inline -inline foo

  $ cat > inline.cat <<'EOF'
  > let foo = po
  > let baz = foo
  > let bar = baz;rf?
  > EOF
  $ check_inline -inline foo -inline baz

  $ cat > inline.cat <<'EOF'
  > let foo = po
  > let bar = foo+
  > EOF
  $ cat > explicit.cat <<'EOF'
  > let bar = po+
  > EOF
  $ check_inline -inline foo

  $ cat > inline.cat <<'EOF'
  > let foo = po | rf
  > let bar = foo+
  > EOF
  $ cat > explicit.cat <<'EOF'
  > let bar = (po | rf)+
  > EOF
  $ check_inline -inline foo

  $ cat > inline.cat <<'EOF'
  > let foo = [M]
  > let bar = foo;rf
  > EOF
  $ cat > explicit.cat <<'EOF'
  > let bar = [M];rf
  > EOF
  $ check_inline -inline foo
  $ check_inline -inline foo -flatten false

Relation references inside event-set definitions are expanded too.

  $ cat > inline.cat <<'EOF'
  > let foo = po
  > let Exp = range(foo)
  > EOF
  $ cat > explicit.cat <<'EOF'
  > let Exp = range(po)
  > EOF
  $ check_inline -inline foo -show Exp -tex catdefinitions.tex

Named checks use the same expansion as definitions.

  $ cat > inline.cat <<'EOF'
  > let foo = po | rf
  > irreflexive foo;co as bar
  > EOF
  $ cat > explicit.cat <<'EOF'
  > irreflexive (po | rf);co as bar
  > EOF
  $ check_inline -inline foo

Rebinding a built-in must fail instead of capturing its earlier meaning or
recursively expanding a non-recursive definition forever.

  $ cat > inline.cat <<'EOF'
  > let po = po & loc
  > let bar = po;rf
  > EOF
  $ miaou7 -q -set-libdir ../libdir -inline po -show bar inline.cat
  References to po precede its definition, cannot inline.
  [1]

  $ cat > inline.cat <<'EOF'
  > let bar = po;co
  > let po = rf
  > EOF
  $ miaou7 -q -set-libdir ../libdir -inline po -show bar inline.cat
  References to po precede its definition, cannot inline.
  [1]

All bindings count towards duplicate detection, including tuple and recursive
bindings and definitions which cannot themselves be inlined.

  $ cat > inline.cat <<'EOF'
  > let foo = po
  > let (foo,baz) = (rf,co)
  > let bar = foo;co
  > EOF
  $ miaou7 -q -set-libdir ../libdir -inline foo -show bar inline.cat
  Multiple definitions of foo, cannot inline.
  [1]

  $ cat > inline.cat <<'EOF'
  > let foo = po
  > let rec foo = rf | foo;rf
  > let bar = foo;co
  > EOF
  $ miaou7 -q -set-libdir ../libdir -inline foo -show bar inline.cat
  Multiple definitions of foo, cannot inline.
  [1]

  $ cat > inline.cat <<'EOF'
  > let rec foo = rf | foo;rf
  > let foo = po
  > EOF
  $ miaou7 -q -set-libdir ../libdir -inline foo -show bar inline.cat
  Multiple definitions of foo, cannot inline.
  [1]

  $ cat > inline.cat <<'EOF'
  > let foo = {}
  > let foo = po
  > EOF
  $ miaou7 -q -set-libdir ../libdir -inline foo -show bar inline.cat
  Multiple definitions of foo, cannot inline.
  [1]

Includes share a binding scope, while independent input files do not.

  $ cat > included.cat <<'EOF'
  > let foo = po
  > EOF
  $ cat > inline.cat <<'EOF'
  > include "included.cat"
  > let bar = foo;rf
  > EOF
  $ cat > explicit.cat <<'EOF'
  > let bar = po;rf
  > EOF
  $ check_inline -inline foo
  $ miaou7 -q -set-libdir ../libdir -inline foo -show bar included.cat inline.cat > actual
  $ miaou7 -q -set-libdir ../libdir -show bar explicit.cat > expected
  $ diff -u expected actual

  $ cat > inline.cat <<'EOF'
  > include "included.cat"
  > let foo = rf
  > EOF
  $ miaou7 -q -set-libdir ../libdir -inline foo -show bar inline.cat
  Multiple definitions of foo, cannot inline.
  [1]
  $ miaou7 -q -set-libdir ../libdir -inline foo -show bar -expand false inline.cat

All traversal modes preserve include order and recursive binding groups.

  $ cat > included.cat <<'EOF'
  > let rec first = po | second;po
  > and second = rf | first;rf
  > EOF
  $ cat > traversal.cat <<'EOF'
  > let start = po;rf
  > include "included.cat"
  > let finish = rf;po
  > irreflexive po;rf as check
  > EOF
  $ miaou7 -set-libdir ../libdir -test true traversal.cat > actual 2> diagnostics
  $ sed -n 's/.*Translating definition //p' diagnostics
  start
  first
  second
  finish
  $ miaou7 -set-libdir ../libdir -test true -expand false traversal.cat > actual 2> diagnostics
  $ sed -n 's/.*Translating definition //p' diagnostics
  start
  finish
  $ miaou7 -set-libdir ../libdir -v -show first traversal.cat > actual 2> diagnostics
  $ sed -n 's/.*Handling definition of //p' diagnostics
  first
  second
  $ miaou7 -set-libdir ../libdir -v -show first -expand false traversal.cat > actual 2> diagnostics
