package io.github.ephemient.aoc2018

class Day3(lines: List<String>) {
    private val input: List<Area> = lines.mapNotNull { it.toArea() }

    private val bounds = run {
        var minFirst = Int.MAX_VALUE
        var minSecond = Int.MAX_VALUE
        var maxFirst = Int.MIN_VALUE
        var maxSecond = Int.MIN_VALUE
        for (area in input) {
            minFirst = minOf(minFirst, area.range.start.first)
            minSecond = minOf(minSecond, area.range.start.second)
            maxFirst = maxOf(maxFirst, area.range.endInclusive.first)
            maxSecond = maxOf(maxSecond, area.range.endInclusive.second)
        }
        IntPair(minFirst, minSecond)..IntPair(maxFirst, maxSecond)
    }

    fun part1(): Int {
        val cloth = IntArray(bounds.size)
        var overlaps = 0
        for (area in input) {
            for (p in area.range) {
                val i = bounds.indexOf(p)
                if (cloth[i]++ == 1) {
                    overlaps++
                }
            }
        }
        return overlaps
    }

    fun part2(): Int? {
        val cloth = IntArray(bounds.size)
        var ids = input.mapTo(mutableSetOf()) { it.id }
        for (area in input) {
            for (p in area.range) {
                val i = bounds.indexOf(p)
                val id = cloth[i]
                if (id == 0) {
                    cloth[i] = area.id
                } else {
                    ids.remove(id)
                    ids.remove(area.id)
                }
            }
        }
        return ids.firstOrNull()
    }

    private data class Area(val id: Int, val range: IntPairRange)

    companion object {
        private val PATTERN = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".toRegex()

        private fun String.toArea(): Area? {
            val match = PATTERN.matchEntire(this) ?: return null
            val (sid, sx, sy, sw, sh) = match.destructured
            val id = sid.toInt()
            val x = sx.toInt()
            val y = sy.toInt()
            val w = sw.toInt()
            val h = sh.toInt()
            return Area(id, IntPair(x, y)..IntPair(x + w - 1, y + h - 1))
        }
    }
}
