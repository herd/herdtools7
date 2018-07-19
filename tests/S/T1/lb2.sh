#! /bin/bash

cat > LB3/lb3$1$2$3$4.litmus <<-EOF
C lb3$1$2$3$4

{atomic_int x0 = 0;}

P0 (atomic_int* y,atomic_int* x, atomic_int* x0) {
  int r0 = atomic_compare_exchange_strong_explicit(x,x0,2,memory_order_$1, memory_order_$2);
  atomic_store_explicit(y,1,memory_order_relaxed);
}

P1 (atomic_int* y,atomic_int* x, atomic_int* x0) {
  int r0 = atomic_load_explicit(y,memory_order_relaxed);
  int r1 = atomic_compare_exchange_strong_explicit(x,x0,1,memory_order_$3, memory_order_$4);
}

exists
(x=2 /\ 0:r0=1 /\ 1:r0=1 /\ 1:r1=0)

EOF
