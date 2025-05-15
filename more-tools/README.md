# Notes

## From Arm Arm to text

On a MacBook Pro:
  1. Open PDF with Acrobat reader, save as Text
  2. Concever to asciii with `iconv -f utf-8 -t ascii//TRANSLIT`
  3. Change `\r` into `\n` with `awk -v RS='\r' -v ORS='\n' '1' `
  4. Remove empty lines (optional) with `awk '1' RS=''`

On Unix, from the text file produced by acrobat reader :
  1. Translate CR to LF : `recode  CR..cl < A.txt | dos2unix > B.txt`
  2. Get rid of utf8 `iconv -f utf-8 -t ascii//TRANSLIT < B.txt > C.txt`
  3. Get Rid of headers and footers : `grep -v -e '^L.a' -e '^ARM DDI' < C.txt > D.txt`
  
## Build dictionary
  Requires pandoc
  1. sh ./build-dict.sh > dict.sh
