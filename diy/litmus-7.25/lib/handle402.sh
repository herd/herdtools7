DIR=$(dirname $0)

if [ $(ocaml $DIR/check402.ml) = ok ]; then
    rm -f $DIR/bytes.ml $DIR/bytes.mli
else
    cp $DIR/bytes.mlp $DIR/bytes.ml
    cp $DIR/bytes.mlip $DIR/bytes.mli
fi
