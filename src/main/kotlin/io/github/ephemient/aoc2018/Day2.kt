package io.github.ephemient.aoc2018

class Day2(private val lines: List<String>) {
    fun part1(): Int {
        val counts = lines.map { it.groupingBy { it }.eachCount().values }
        return counts.count { 2 in it } * counts.count { 3 in it }
    }

    fun part2(): String? {
        for ((i, line) in lines.withIndex()) {
            for (j in i + 1..lines.lastIndex) {
                line.zip(lines[j]) { a, b -> a.takeIf { it == b } }
                    .takeIf { it.count { it == null } == 1 }
                    ?.let { return it.filterNotNull().joinToString(separator = "") }
            }
        }
        return null
    }
}
