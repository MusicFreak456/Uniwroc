if [ $# -gt 1 ] || ( [ $# = 1 ] && [ $1 -lt 2 ] ); then
  printf "Usage: %s [n]\nwhere n > 2\n" $0
  exit
fi

MAX=1000

if [ $# = 1 ]; then
  MAX=$1
fi

function filter {
  n=$1
  while read x; do
    if [ $(( x % n )) != 0 ]; then
      echo $x
    fi
  done
}

function stage {
  if read n; then
    echo $n
    filter $n | stage&
  fi
}

seq 2 $MAX | stage | cat
