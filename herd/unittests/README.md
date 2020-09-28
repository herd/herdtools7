
This repository contains unittests for the implementations of architectures in herd.

These are intended as sanity checks to ensure that the instruction act as originally intended. This is by no means exhaustive.

As a principle, each time you extend an architecture you should add unit tests for all instruction variants you touch.

The tests should return Always, that is a precise state/set of states when run with, for instance:

For AArch64
```
herd7 -model aarch64.cat A**.litmus
```


