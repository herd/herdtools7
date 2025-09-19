

CATFILES := $(shell find ../../../catalogue/ ../../../herd/libdir/ -name '*.cat' -type f)

TARGETS_BASE := $(patsubst %.cat,%,$(patsubst ../../../%,%,$(CATFILES)))
FAILING_BASE := \
  catalogue/demo/cats/x86                          \
  catalogue/demo/cats/arm                          \
  catalogue/demo/cats/c11                          \
  catalogue/demo/cats/ptx                          \
  catalogue/hrf/cats/hrfd                          \
  catalogue/hrf/cats/hrfi                          \
  catalogue/hrf/cats/hrfir                         \
  catalogue/hrf/cats/hrfdr                         \
  catalogue/c11popl15/alternatives                 \
  catalogue/aarch64-VMSA/cats/alternative-common   \
  catalogue/aarch64-VMSA/cats/egc                  \
  catalogue/aarch64-VMSA/cats/ec                   \
  catalogue/linux/cats/linux-kernel                \
  herd/libdir/uniproc-normw-cat


TARGETS := $(patsubst %,output/%/ok,$(TARGETS_BASE))
FAILING := $(patsubst %,output/%/ok,$(FAILING_BASE))
PASSING := $(filter-out $(FAILING),$(TARGETS))



BINDIR := $(shell cd ../bin; pwd)
export PATH := $(BINDIR):$(PATH)

passing : $(PASSING)

all : $(TARGETS)

output/%/ok : ../../../%.cat
	test-ast.sh $< output/$*

clean :
	rm -rf output
