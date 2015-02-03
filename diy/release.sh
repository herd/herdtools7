set -e
DIR=`dirname $0`
. $DIR/version.sh
. $DIR/funs.sh
TMP=/var/tmp
FINAL=$TMP/diy-$V
EXPORT=$FINAL
/bin/rm -rf $FINAL && mkdir $FINAL
(
  extract litmus litmus && \
  extract gen gen && \
  extract herd herd && \
  extract tools tools && \
  extract lib lib && \
  extract diy/Makefile.herd Makefile && \
  extract diy/README.txt README.txt && \
  extract diy/INSTALL.txt INSTALL.txt && \
  extract diy/LICENSE.txt  LICENSE.txt && \
  true
)
( cleandir $FINAL/litmus ) && \
( cleandir $FINAL/gen ) && \
( cleandir $FINAL/herd ) && \
( cleandir $FINAL/tools )

# add example and documentation
sh $DIR/installdoc.sh $FINAL
NAME=`basename $FINAL`
( cd $TMP && tar cf - $NAME ) | gzip --best > $NAME.tar.gz
/bin/rm -rf $FINAL
