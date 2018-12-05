package io.github.ephemient.aoc2018

class Day5(lines: List<String>) {
    private val input = lines.first()

    fun part1(): Int = react()

    fun part2(): Int? = ('A'..'Z').map { react(it) }.min()

    private val buffer = CharArray(input.length)

    private fun react(exclude: Char? = null): Int {
        var pos = 0
        for (c in input) {
            if (exclude?.equals(c, ignoreCase = true) == true) continue
            if (pos > 0 && buffer[pos - 1].let { it != c && it.equals(c, ignoreCase = true) }) {
                pos--
            } else {
                buffer[pos++] = c
            }
        }
        return pos
    }
}
