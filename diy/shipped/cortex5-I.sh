SRC="../../mem.new/tests/extra-arm/@EX"
NAME=cortex5-I
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
litmus -mach cortex9 -s 1k -r 1k -a 2 -limit true -p 0,1 -i 1 -st 5 -o $NAME.tgz $SRC
pack $NAME
scp $NAME.tgz roussette:/var/tmp
ssh -q -T -n -x roussette "cd /var/tmp && tar zxf /var/tmp/$NAME.tgz && cd /var/tmp/$NAME && make -s && cd .. && tar zcf /var/tmp/$NAME.tgz $NAME"
scp roussette:/var/tmp/$NAME.tgz .


