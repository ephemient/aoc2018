package io.github.ephemient.aoc2018

class Day5(lines: List<String>) {
    private val input = lines.single()

    fun part1(): Int = react(input).length

    fun part2(): Int? {
        val prepared = react(input)
        return ('A'..'Z').map { react(prepared, it).length }.min()
    }

    private val buffer = CharArray(input.length)

    companion object {
        private fun react(input: String, exclude: Char? = null): String {
            val buffer = CharArray(input.length)
            var pos = 0
            for (c in input) {
                if (exclude?.equals(c, ignoreCase = true) == true) continue
                if (pos > 0 && buffer[pos - 1].let { it != c && it.equals(c, ignoreCase = true) }) {
                    pos--
                } else {
                    buffer[pos++] = c
                }
            }
            return buffer.joinToString(separator = "", limit = pos, truncated = "")
        }
    }
}
