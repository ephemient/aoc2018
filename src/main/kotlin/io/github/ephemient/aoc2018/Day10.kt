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
        val times = mutableMapOf<Int, IntHolder>()
        for ((i, p1) in points.withIndex()) {
            val (x1, y1, dx1, dy1) = p1
            for (j in i + 1..points.lastIndex) {
                val (x2, y2, dx2, dy2) = points[j]
                if (dx1 != dx2 && (x1 - x2) % (dx2 - dx1) == 0) {
                    times.getOrPut((x1 - x2) / (dx2 - dx1)) { IntHolder() }.value++
                }
                if (dy1 != dy2 && (y1 - y2) % (dy2 - dy1) == 0) {
                    times.getOrPut((y1 - y2) / (dy2 - dy1)) { IntHolder() }.value++
                }
            }
        }
        val t = requireNotNull(times.entries.maxBy { it.value.value }).key
        var minX = Int.MAX_VALUE
        var maxX = Int.MIN_VALUE
        var minY = Int.MAX_VALUE
        var maxY = Int.MIN_VALUE
        val results = points.mapTo(mutableSetOf<IntPair>()) { point ->
            val x = point.x + point.dx * t
            val y = point.y + point.dy * t
            if (x < minX) minX = x
            if (x > maxX) maxX = x
            if (y < minY) minY = y
            if (y > maxY) maxY = y
            IntPair(point.x + point.dx * t, point.y + point.dy * t)
        }
        _part1 = (minY..maxY).joinToString(separator = "\n") { y ->
            (minX..maxX).joinToString(separator = "") { x ->
                if (IntPair(x, y) in results) "▓" else "░"
            }
        }
        _part2 = t
    }

    fun part1(): String = _part1

    fun part2(): Int = _part2

    private data class Point(val x: Int, val y: Int, val dx: Int, val dy: Int)

    private data class IntHolder(var value: Int = 0)

    companion object {
        private val PATTERN =
            """position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>""".toRegex()
    }
}
