# Notes

## From Arm Arm to text

On a MacBook Pro:
  1. Open PDF with Acrobat reader, save as Text
  2. Concever to asciii with `iconv -f utf-8 -t ascii//TRANSLIT`
  3. Change `\r` into `\n` with `awk -v RS='\r' -v ORS='\n' '1' `
  4. Remove empty lines (optional) with `awk '1' RS=''`

## Build dictionary
  Requires pandoc
  1. sh ./build-dict.sh > dict.sh
