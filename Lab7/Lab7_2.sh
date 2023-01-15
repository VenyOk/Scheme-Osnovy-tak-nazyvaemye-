#!/bin/bash

count_lines() {
    grep -cve "^\s*$" "$1"
}

counter=0
while read -r file; do
    (( counter += $(count_lines "$file") ))
done <<< "$(find "$1" -name "*.c" -o -name "*.h")"
echo "$counter"
