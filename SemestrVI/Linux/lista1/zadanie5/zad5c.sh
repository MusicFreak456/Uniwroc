#!/bin/bash

doc_dir="/usr/share/doc"

installed=$(\
  dpkg-query -W -f='${Package} ${Status}\n' \
  | grep installed \
  | cut -d' ' -f1 \
  | sort \
  | uniq)

doc_present=$(ls -1 $doc_dir | sort)

comm -12 <(echo "${installed}") <(echo "${doc_present}") |
while read line 
do
  changelog="${doc_dir}/${line}/changelog.Debian.gz"
  if !(test -f $changelog); then 
    echo "${line}"
  fi
done
