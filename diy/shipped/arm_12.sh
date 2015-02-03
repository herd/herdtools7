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
litmus -mach ARM -a 2 -limit true -o ARM1.tgz $SRC
pack ARM1
litmus -mach ARM -o ARM2.tgz $SRC
pack ARM2
