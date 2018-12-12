package io.github.ephemient.aoc2018

class Day12(lines: List<String>) {
    private val initial = lines.first().filter { it in "#." }

    private val mappings: Map<CharSequence, CharSequence> = lines.drop(2).associate { line ->
        val (key, value) = line.split(" => ", limit = 2)
        key to value
    }

    private fun solve(n: Long): Long {
        val seen = mutableMapOf<String, Pair<Long, Long>>()
        var offset = 0L
        var state = initial
        var i = 0L
        while (i < n) {
            seen[state] = i to offset
            state = "....$state....".windowed(5) { mappings.getOrElse(it) { "." } }
                .joinToString(separator = "")
                .dropLastWhile { it != '#' }
                .apply { offset += indexOf('#') - 2 }
                .dropWhile { it != '#' }
            i++
            val (previousI, previousOffset) = seen[state] ?: continue
            offset += (n - i) / (i - previousI) * (offset - previousOffset)
            i = n - (n - i) % (i - previousI)
        }
        return state.withIndex().fold(0L) { acc, (i, c) -> if (c == '#') acc + offset + i else acc }
    }

    fun part1(): Long = solve(20)

    fun part2(): Long = solve(50000000000)
}
