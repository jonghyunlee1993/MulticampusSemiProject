find "$1" -name "*.php" -o -name "*.html" -o -name "*.jpg" -o -name "*.png" -o -name "*.gif" -o -name "*.GIF" -o -name "*.JPG" -o -name "*.PNG" -o -name "css" -o -name "*.js" -o -name "*.txt" -o -name "*.csv" | while read filename

do

tempName=${filename}~temp~.txt

mv "$filename" "$tempName"

iconv -c -f euc-kr -t utf-8 "$tempName" > "$filename"

rm "$tempName"


done
