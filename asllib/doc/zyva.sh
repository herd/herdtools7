#! /bin/sh
case "$1" in
    "fix0")
        FIX=false
        ;;
    ""|"fix")
        FIX=true
        ;;
    *)
        echo "Usage: sh ./zyva [fix|fix0]" 1>&2
        exit 1
        ;;
esac

mkdir -p HTML
make  generated_spec
hevea -fix0 -O -o HTML/ASL.html book.hva macros.hva ASLReference.tex
bibhva HTML/ASL
if $FIX
then
    hevea -fix -O -o HTML/ASL.html book.hva macros.hva ASLReference.tex
fi
( cd HTML && hacha ASL.html )
