An explicit comma stays a sequence after a choice in `diy7`
  $ diy7 -arch AArch64 -mode free -filter-check 'PodWR|Rfe,Fre' PodWR
  Sequence `[PodWR,Fre]` `PodWR` passes the internal filter in mode `free`
  Sequence `[Rfe,Fre]` `PodWR` passes the internal filter in mode `free`
