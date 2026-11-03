Use litmus7 to generate AArch64 self-modifying code in standard and PreSi modes

The standard-mode checks cover the `self` variant and the shared-library
files emitted for a plain test.

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

Non-KVM PreSi uses the shared self library when the `self` variant is enabled,
and still emits the library files without that variant.

  $ TEST="PreSiSelfShell"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -mach aarch64 -mode presi -a 1 -s 1 -r 1 -o "$TEST" -variant self -driver shell "Self.litmus"
  $ test -f "$TEST/litmus/self.c" && test -f "$TEST/litmus/self.h"
  $ grep -qFx '#include <self.h>' "$TEST/Self.c"
  $ grep -qF 'const uintptr_t line_size = cache_line_size();' "$TEST/Self.c"
  $ grep -qF 'if (!check_dic_idc(0, 0)) return 0;' "$TEST/Self.c"
  $ ! grep -qF 'static uint32_t cache_line_size' "$TEST/Self.c"
  $ ! grep -qF 'inline static void selfbar' "$TEST/Self.c"
  $ ! grep -qF 'cache_line_size = getcachelinesize();' "$TEST/Self.c"
  $ grep -qF '$(SHARED_LIB): $(SHARED_OBJ)' "$TEST/Makefile"
  $ grep -qF '$(GCC) $(GCCOPTS) $(LINKOPTS) -o $@ $(UTILS) $< $(SHARED_LIB)' "$TEST/Makefile"

  $ TEST="PreSiPlainShell"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -mach aarch64 -mode presi -a 1 -s 1 -r 1 -o "$TEST" -driver shell "Self.litmus"
  $ test -f "$TEST/litmus/self.c" && test -f "$TEST/litmus/self.h"
  $ ! grep -qF '#include <self.h>' "$TEST/Self.c"
  $ grep -qF '$(SHARED_LIB): $(SHARED_OBJ)' "$TEST/Makefile"

The PreSi C driver also links the archive after its generated objects for both
variants.

  $ TEST="PreSiSelfC"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -mach aarch64 -mode presi -a 1 -s 1 -r 1 -o "$TEST" -variant self -driver C "Self.litmus"
  $ test -f "$TEST/litmus/self.c" && test -f "$TEST/litmus/self.h"
  $ grep -qFx '#include <self.h>' "$TEST/Self.c"
  $ grep -qF '$(GCC)  $(GCCOPTS) $(LINKOPTS) -o $@ $(UTILS) @obj run.o $(SHARED_LIB)' "$TEST/Makefile"

  $ TEST="PreSiPlainC"
  $ mkdir "$TEST"
  $ litmus7 -set-libdir ../../libdir -mach aarch64 -mode presi -a 1 -s 1 -r 1 -o "$TEST" -driver C "Self.litmus"
  $ test -f "$TEST/litmus/self.c" && test -f "$TEST/litmus/self.h"
  $ ! grep -qF '#include <self.h>' "$TEST/Self.c"
  $ grep -qF '$(GCC)  $(GCCOPTS) $(LINKOPTS) -o $@ $(UTILS) @obj run.o $(SHARED_LIB)' "$TEST/Makefile"
