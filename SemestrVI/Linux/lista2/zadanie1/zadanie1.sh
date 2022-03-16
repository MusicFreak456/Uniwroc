
if [ $# -gt 1 ]; then
  printf "Usage: %s [path]" $0
  exit
fi

S_PATH="."

if [ $# = 1 ]; then
  S_PATH=$1
fi

IFS=$'\n'

FILES=$(find music -regextype egrep -regex ".*.mp3$")
FILES_ARRAY=( $FILES )

select ITEM in $(xargs -d "\n" -L1 mp3info -p "%l(%a): %t\n" <<< $FILES)
do
  if [ $REPLY -gt ${#FILES_ARRAY[@]} ] || [ $REPLY -lt 1 ]; then
    echo "Index out of bounds"
    continue
  fi
  INDEX=$((REPLY - 1))
  mplayer -quiet ${FILES_ARRAY[$INDEX]} > /dev/null 2> /dev/null
done
