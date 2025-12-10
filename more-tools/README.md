# Notes

## From Arm Arm to text

On a MacBook Pro:
  1. Open PDF with Acrobat reader, save as Text in A.txt.
  2. Change `\r` into `\n` with `awk -v RS='\r' -v ORS='\n' '1' `
  3. Convert to asciii with `iconv -c -f utf-8 -t ascii//TRANSLIT`
  4. Get Rid of headers and footers: `awk -f headers.awk`.
  5. Extract definitions in the current directory
     with the extractdefs tool: `extractdefs7 -o . D.txt`

On Unix, from the text file `A.txt` produced by acrobat reader on an
Apple machine:
  1. Translate CR to LF : `recode  CR..cl < A.txt | dos2unix > B.txt`.
  2. Get rid of utf8 `iconv -c -f utf-8 -t ascii//TRANSLIT < B.txt > C.txt`.
  3. Get Rid of headers and footers: `awk -f headers.awk <C.txt > D.txt`.
  4. Extract definitions in the current directory
     with the extractdefs tool: `extractdefs7 -o . D.txt`

## Build dictionary
  Requires pandoc
  1. sh ./build-dict.sh > dict.sh
