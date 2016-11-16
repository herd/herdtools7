#General behaviour
arch = X86
mode = conform
stabilise = 1
#Cycle control
safe = Rfe,Fre,Wse,Pod**,[Rfi,PodRR]
nprocs = 2
#External tool control
litmus_opts = -a 2 -i 0
run_opts = -s 100000 -r 10,-s 5000 -r 200 -i 1
build = make -j 2 -s
