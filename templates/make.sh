#!/bin/bash

mkdir $1

find . -name '*.hs' | while read line; do
  line=$(basename $line)
  echo "Working on $line"
  sed -e "s/<newmodule>/$1/g" "$line" > "$1/$line"
  cat "$1/$line"
done
  
echo "Completed. New exchange boilerplate created."


