mkdir -p HTML
make  generated_spec
hevea -fix0 -O -o HTML/ASL.html book.hva macros.hva ASLReference.tex
bibhva HTML/ASL
if false
then
    hevea -fix -O -o HTML/ASL.html book.hva macros.hva ASLReference.tex
fi
( cd HTML && hacha ASL.html )
