cat $* | \
while read name kind rem
do
  echo $(basename $name .c) $kind
done
