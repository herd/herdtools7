rm -rf HTML
mkdir HTML
make  generated_spec
hevea -fix -O -o HTML/ASL.html book.hva macros.hva ASLReference.tex
( cd HTML && hacha ASL.html )
