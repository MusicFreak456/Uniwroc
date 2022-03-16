#!/bin/bash

installed=$(\
  dpkg-query -W -f='${Package} ${Status}\n' \
  | grep installed \
  | cut -d' ' -f1 \
  | sort \
  | uniq)
doc_present=$(ls -1 /usr/share/doc | sort)
comm -13 <(echo "${installed}") <(echo "${doc_present}") |
while read line 
do
  echo "${line} $(dpkg-query -S "/usr/share/doc/${line}" | cut -d':' -f1)"
done
