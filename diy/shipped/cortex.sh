SRC=../../mem.new/PLDI-ARM/@all
pack () {
  BASE=$1
  TAR=$BASE.tgz
  /bin/rm -rf $BASE &&\
  mkdir $BASE &&\
  cd $BASE &&\
  tar zxmf ../$TAR &&\
  cd .. &&\
  tar zcf $TAR $BASE &&\
  /bin/rm -rf $BASE
}
litmus -mach cortex9 -a 2 -limit true -o cortex.tgz $SRC
pack cortex
