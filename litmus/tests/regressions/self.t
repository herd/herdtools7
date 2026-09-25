Use litmus7 to generate AArch64 self-modifying code

This test checks generation for the `self` variant only.

  $ TEST="Self"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -mach aarch64 -o "$TEST" \
  > -mode std -a 1 -s 1 -r 1 \
  > -variant self -driver shell \
  > "Self.litmus"

The generated test calls the support-library API instead of including its
implementation, and the shell-driver Makefile compiles and links that utility.

  $ test -f "$TEST/litmus/self.c"
  $ test -f "$TEST/litmus/self.h"
  $ grep -Fx '#include <self.h>' "$TEST/Self.c"
  #include <self.h>
  $ grep -Fx 'SHARED_SRC_DIR=$(CURDIR)/litmus' "$TEST/Makefile"
  SHARED_SRC_DIR=$(CURDIR)/litmus
  $ grep -Fx 'SHARED_OBJ=$(SHARED_SRC:.c=.o)' "$TEST/Makefile"
  SHARED_OBJ=$(SHARED_SRC:.c=.o)
  $ grep -Fx 'SHARED_LIB=$(SHARED_SRC_DIR)/liblitmus.a' "$TEST/Makefile"
  SHARED_LIB=$(SHARED_SRC_DIR)/liblitmus.a

The support library is also emitted for AArch64 tests that do not enable the
`self` variant; the variant only controls use of its API.

  $ TEST="Plain"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -mach aarch64 -o "$TEST" \
  > -mode std -a 1 -s 1 -r 1 \
  > -driver shell \
  > "Self.litmus"
  $ test -f "$TEST/litmus/self.c"
  $ test -f "$TEST/litmus/self.h"
  $ ! grep -F '#include <self.h>' "$TEST/Self.c"

The C driver uses the same support library.

  $ TEST="SelfC"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -mach aarch64 -o "$TEST" \
  > -mode std -a 1 -s 1 -r 1 \
  > -variant self -driver C \
  > "Self.litmus"
  $ test -f "$TEST/litmus/self.c"
  $ test -f "$TEST/litmus/self.h"
  $ grep -Fx '#include <self.h>' "$TEST/Self.c"
  #include <self.h>
  $ grep -Fx 'SHARED_SRC_DIR=$(CURDIR)/litmus' "$TEST/Makefile"
  SHARED_SRC_DIR=$(CURDIR)/litmus
  $ grep -Fx 'SHARED_OBJ=$(SHARED_SRC:.c=.o)' "$TEST/Makefile"
  SHARED_OBJ=$(SHARED_SRC:.c=.o)
