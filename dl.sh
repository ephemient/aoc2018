#!/bin/bash
set -euxo pipefail
YEAR=${2:-2018}
DAY=${1}
curl -b "session=$(<~/.aocrc)" -o "day${DAY}.txt" "https://adventofcode.com/${YEAR}/day/${DAY}/input"
