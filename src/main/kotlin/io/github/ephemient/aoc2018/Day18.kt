package io.github.ephemient.aoc2018

class Day18(private val lines: List<String>) {
    fun part1(): Int = solve(10)

    fun part2(): Int = solve(1000000000)

    private fun solve(target: Long): Int {
        val cache = mutableMapOf<List<String>, Long>()
        var n = 0L
        var goal = target
        var state = lines
        while (n < goal) {
            val m = cache.getOrPut(state) { n }
            if (m < n) {
                goal = n + 1 + (goal - n - 1) % (n - m)
            }
            state = state.mapIndexed { y, row ->
                row.withIndex().joinToString(separator = "") { (x, current) ->
                    val window = (maxOf(0, y - 1)..minOf(state.lastIndex, y + 1)).flatMap { y ->
                        (maxOf(0, x - 1)..minOf(row.lastIndex, x + 1)).map { x -> state[y][x] }
                    }
                    when {
                        current == '.' && window.count { it == '|' } >= 3 -> '|'
                        current == '|' && window.count { it == '#' } >= 3 -> '#'
                        current == '#' &&
                            (window.count { it == '|' } < 1 || window.count { it == '#' } < 2) ->
                            '.'
                        else -> current
                    }.toString()
                }
            }
            n++
        }
        return state.sumBy { it.count { it == '|' } } * state.sumBy { it.count { it == '#' } }
    }
}
