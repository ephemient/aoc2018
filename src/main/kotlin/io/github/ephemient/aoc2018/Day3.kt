package io.github.ephemient.aoc2018

class Day3(lines: List<String>) {
    private val input: List<Area> = lines.mapNotNull { it.toAreaOrNull() }

    fun part1(): Int {
        val cloth = mutableMapOf<IntPair, Int>()
        var overlaps = 0
        for (area in input) {
            for (p in area.range) {
                val n = cloth.getOrElse(p) { 0 }
                cloth[p] = n + 1
                if (n == 1) overlaps++
            }
        }
        return overlaps
    }

    fun part2(): Int? {
        val cloth = mutableMapOf<IntPair, Int>()
        var ids = input.mapTo(mutableSetOf()) { it.id }
        for (area in input) {
            for (p in area.range) {
                val id = cloth.getOrPut(p) { area.id }
                if (id != area.id) {
                    ids.remove(id)
                    ids.remove(area.id)
                }
            }
        }
        return ids.singleOrNull()
    }

    private data class Area(val id: Int, val range: IntPairRange)

    companion object {
        private val PATTERN = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".toRegex()

        private fun String.toAreaOrNull(): Area? {
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
