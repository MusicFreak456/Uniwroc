
for i in {1..8}
do
    echo "man$i:"
    find /usr/share/man/ -regextype egrep -regex ".*$i.gz$" \
    | sed -r 's:man/man:man/en/man:' \
    | cut -d/ -f5 \
    | sort \
    | uniq
    echo
done
