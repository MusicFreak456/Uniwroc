#!/bin/bash

installed=$(\
  dpkg-query -W -f='${Package} ${Status}\n' \
  | grep installed \
  | cut -d' ' -f1 \
  | sort \
  | uniq)
doc_present=$(ls -1 /usr/share/doc | sort)
comm -23 <(echo "${installed}") <(echo "${doc_present}")
