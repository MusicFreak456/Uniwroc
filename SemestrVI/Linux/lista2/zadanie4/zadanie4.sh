
if [ $# -lt 1 ] || [ $# -gt 1 ] || [ $1 = "-h" ] || [ $1 = "--help" ]; then
  echo "usage: $0 [--help | -h] <max>"
  exit 1
fi

MAX=$1

declare -a PRIMES
PRIMES[0]=2
i=1
n=3

while [ $n -le $MAX ]; do
  j=0
  while [ $(( PRIMES[j] * PRIMES[j] )) -le $n ]; do
    if [ $(( n % PRIMES[j] )) -eq 0 ]; then
      (( n++ ))
      continue 2
    fi
    (( j++ ))
  done
  PRIMES[$i]=$n
  (( i++ ))
  (( n++ ))
done

echo ${PRIMES[*]}
