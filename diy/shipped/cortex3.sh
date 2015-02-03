SRC=../../mem.new/PLDI-ARM/@all2
NAME=cortex3
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
litmus -mach cortex9 -mem direct -a 2 -limit true -o $NAME.tgz $SRC
pack $NAME
scp $NAME.tgz roussette:/var/tmp
ssh -q -T -n -x roussette "cd /var/tmp && tar zxf /var/tmp/$NAME.tgz && cd /var/tmp/$NAME && make -s && cd .. && tar zcf /var/tmp/$NAME.tgz $NAME"
scp roussette:/var/tmp/$NAME.tgz .


