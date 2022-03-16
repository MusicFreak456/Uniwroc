
for man in man{1..8}
do
    echo "${man}:"
    find /usr/share/man/pl/$man -type f -printf "%f\n" 2>/dev/null | cut -d. -f1
    echo
done
