package io.github.ephemient.aoc2018

class Day10(lines: List<String>) {
    private val _part1: String
    private val _part2: Int

    init {
        val points = lines.also { require(it.isNotEmpty()) }
            .map { line ->
                val (x, y, dx, dy) = requireNotNull(PATTERN.matchEntire(line)).destructured
                Point(x.toInt(), y.toInt(), dx.toInt(), dy.toInt())
            }
        val (i, result, bounds) = generateSequence(points) { it.map { it.step() } }
            .map { it to checkNotNull(it.bounds()) }
            .zipWithNext()
            .withIndex()
            .first { (_, pair) -> pair.first.second.sizeAsLong < pair.second.second.sizeAsLong }
            .let { (i, pair) -> Triple(i, pair.first.first, pair.first.second) }
        val (x0, y0) = bounds.start
        val (x1, y1) = bounds.endInclusive
        _part1 = with(Array(y1 - y0 + 1) { BooleanArray(x1 - x0 + 1) }) {
            for ((x, y) in result) this[y - y0][x - x0] = true
            joinToString(separator = "\n") {
                it.joinToString(separator = "") { if (it) "\u2593" else "\u2591" }
            }
        }
        _part2 = i
    }

    fun part1(): String = _part1

    fun part2(): Int = _part2

    private data class Point(val x: Int, val y: Int, val dx: Int, val dy: Int) {
        fun step() = copy(x = x + dx, y = y + dy)
    }

    companion object {
        private val PATTERN =
            """position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>""".toRegex()

        private fun Iterable<Point>.bounds() = fold<Point, IntPairRange?>(null) { range, (x, y) ->
            range?.takeIf { IntPair(x, y) in it } ?: IntPair(
                if (range == null) x else minOf(range.start.first, x),
                if (range == null) y else minOf(range.start.second, y)
            )..IntPair(
                if (range == null) x else maxOf(range.endInclusive.first, x),
                if (range == null) y else maxOf(range.endInclusive.second, y)
            )
        }
    }
}
