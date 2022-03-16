#!/bin/bash

lib_regex=".*\.so\(\.[0-9]\+\)\?$"

number=0
size=0
libs=$(find / -regextype grep -regex $lib_regex -printf "%s\n" 2>/dev/null)
while read line; do
  ((number+=1))
  ((size+=line))
done <<< "$libs"
  
echo "Number: ${number}"
echo "Total size: ${size}"
echo "Average: $(expr $size / $number)"
