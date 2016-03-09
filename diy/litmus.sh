set -e
DIR=`dirname $0`
. $DIR/version.sh
. $DIR/funs.sh
TMP=/var/tmp
NAME=litmus-$V
FINAL=$TMP/$NAME
EXPORT=$FINAL
/bin/rm -rf $FINAL && mkdir $FINAL
( 
  extract litmus $NAME && \
  extract diy/LICENSE.txt $NAME/LICENSE.txt && \
  ( cd $EXPORT/$NAME && /bin/rm lib ) && \
  extract lib  $NAME/lib && \
  true
)
( cleandir $FINAL/$NAME )
( cd $EXPORT && tar zcf $NAME.tar.gz $NAME )
( mv $EXPORT/$NAME.tar.gz . && /bin/rm -rf $EXPORT )
cp $NAME.tar.gz $DEST/old
( cd $DEST && ln -sf old/$NAME.tar.gz litmus.tar.gz )
