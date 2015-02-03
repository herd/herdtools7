set -e
DIR=`dirname $0`
TMP=/var/tmp
. $DIR/version.sh
. $DIR/funs.sh
NAME=herd-$V
EXPORT=$TMP/export.$$
FINAL=$EXPORT/$NAME
mkdir -p $EXPORT
(
  extract herd $NAME && \
  extract diy/LICENSE.txt $NAME//LICENSE.txt && \
  ( cd $EXPORT/$NAME && /bin/rm lib ) && \
  extract lib $NAME/lib && \
  true
)
#TMPF=/tmp/$$.txt
#( cd $FINAL && sed -e 's|MCYCLES=.*|MCYCLES=|' Makefile > $TMPF && mv $TMPF Makefile )
( cleandir $FINAL )
( cd $EXPORT && tar zcf $NAME.tar.gz $NAME )
( mv $EXPORT/$NAME.tar.gz . && /bin/rm -rf $EXPORT )
