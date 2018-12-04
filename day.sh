#!/bin/bash
. "$(git --exec-path)/git-sh-setup" || exit $?
set -euxo pipefail
DAY=${1}
grep -F -w -e "Day${DAY}" -q src/main/kotlin/io/github/ephemient/aoc2018/main.kt || sed -i -e "
    \$ i \\
\\
    if (days?.contains(${DAY}) != false) {\\
        val day${DAY} = Day${DAY}(resources[\"day${DAY}.txt\"])\\
        println(\"Day ${DAY}\")\\
        println(day${DAY}.part1())\\
        println(day${DAY}.part2())\\
        println()\\
    }
" src/main/kotlin/io/github/ephemient/aoc2018/main.kt
[[ -e "src/main/kotlin/io/github/ephemient/aoc2018/Day${DAY}.kt" ]] || cat >"src/main/kotlin/io/github/ephemient/aoc2018/Day${DAY}.kt" <<EOF
package io.github.ephemient.aoc2018

class Day${DAY}(private val lines: List<String>) {
    fun part1(): Int = 0

    fun part2(): Int = 0
}
EOF
[[ -e "src/test/kotlin/io/github/ephemient/aoc2018/Day${DAY}Test.kt" ]] || cat >"src/test/kotlin/io/github/ephemient/aoc2018/Day${DAY}Test.kt" <<EOF
package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day${DAY}Test {
    @Test
    fun \`part 1 examples\`() {
        assertEquals(0, Day${DAY}(emptyList()).part1())
    }

    @Test
    fun \`part 2 examples\`() {
        assertEquals(0, Day${DAY}(emptyList()).part2())
    }
}
EOF
[[ -e "src/jmh/kotlin/io/github/ephemient/aoc2018/Day${DAY}Bench.kt" ]] || cat >"src/jmh/kotlin/io/github/ephemient/aoc2018/Day${DAY}Bench.kt" <<EOF
package io.github.ephemient.aoc2018

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State

@State(Scope.Thread)
open class Day${DAY}Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = javaClass.classLoader.getResourceAsStream("day${DAY}.txt").bufferedReader().readLines()
    }

    @Benchmark
    fun part1(): Int = Day${DAY}(lines).part1()

    @Benchmark
    fun part2(): Int = Day${DAY}(lines).part2()
}
EOF
[[ -e "src/main/resources/day${DAY}.txt" ]] || touch "src/main/resources/day${DAY}.txt"
git add -N "src/main/kotlin/io/github/ephemient/aoc2018/Day${DAY}.kt" "src/test/kotlin/io/github/ephemient/aoc2018/Day${DAY}Test.kt" "src/jmh/kotlin/io/github/ephemient/aoc2018/Day${DAY}Bench.kt" "src/main/resources/day${DAY}.txt"
nohup gvim -p src/main/kotlin/io/github/ephemient/aoc2018/main.kt "src/main/kotlin/io/github/ephemient/aoc2018/Day${DAY}.kt" "src/test/kotlin/io/github/ephemient/aoc2018/Day${DAY}Test.kt" "src/jmh/kotlin/io/github/ephemient/aoc2018/Day${DAY}Bench.kt" "src/main/resources/day${DAY}.txt" +2tabn 0<&- &>/dev/null & disown
./gradlew -t run --args="${DAY}"
