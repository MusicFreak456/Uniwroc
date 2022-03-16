# PORAŻKA
if [ $# -gt 1 ]; then
  printf "Usage: %s [path]" $0
  exit
fi

ENTRIES=$(sed -r '/^#|^[[:blank:]]*$/ d' < /etc/fstab | tr -s '[:blank:]')

function print_entry {
  SAVED_IFS=$IFS
  IFS=" "

  LABELS=("Device:" \
  "Filesystem type:" \
  "Mount options:" \
  "Dump frequency:" \
  "Fsck pass number:")
  FIELDS=( $1 )
  n=${#LABELS[@]}

  IFS=$SAVED_IFS

  for ((i=0; i<n; i++)) do
    printf "%-17.20s %s\n" ${LABELS[i]} ${FIELDS[i]}
  done
  echo
}

IFS=$'\n'
for x in $ENTRIES; do
  print_entry $x
done
