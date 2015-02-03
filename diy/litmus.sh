set -e
DIR=`dirname $0`
TMP=/var/tmp
. $DIR/version.sh
. $DIR/funs.sh
NAME=litmus-$V
EXPORT=$TMP/export.$$
FINAL=$EXPORT/$NAME
mkdir -p $EXPORT
( 
  extract litmus $NAME && \
  extract diy/LICENSE.txt LICENSE.txt && \
  ( cd $EXPORT/$NAME && /bin/rm lib ) && \
  extract lib  $NAME/lib && \
  extract litmus/generated $NAME/generated && \
  true )
( cleandir $FINAL )
( cd $EXPORT && tar zcf $NAME.tar.gz $NAME )
( mv $EXPORT/$NAME.tar.gz . && /bin/rm -rf $EXPORT )