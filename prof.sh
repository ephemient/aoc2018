#!/bin/bash
set -euxo pipefail
DAYS=($*)
[[ ${#DAYS[*]} -gt 0 ]] || DAYS=($(ls day*.txt | grep -o '[[:digit:]][[:digit:]]*'))
BUILD=(build --profile)
for day in ${DAYS[*]}; do
    [[ ${day} -le 0 ]] || BUILD+=(--exec "aoc2018-exe $day +RTS -p -poDay$day")
done
exec stack "${BUILD[@]}"
