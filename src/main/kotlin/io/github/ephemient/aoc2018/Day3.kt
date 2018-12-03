package io.github.ephemient.aoc2018

class Day3(lines: List<String>) {
    private val input: List<Area> = lines.mapNotNull { it.toAreaOrNull() }

    fun part1(): Int {
        val cloth = mutableMapOf<Pair<Int, Int>, Int>()
        var overlaps = 0
        for (area in input) {
            for (x in area.x) {
                for (y in area.y) {
                    val p = x to y
                    val n = cloth.getOrElse(p) { 0 }
                    cloth[p] = n + 1
                    if (n == 1) overlaps++
                }
            }
        }
        return overlaps
    }

    fun part2(): Int? {
        val cloth = mutableMapOf<Pair<Int, Int>, Int>()
        var ids = input.mapTo(mutableSetOf()) { it.id }
        for (area in input) {
            for (x in area.x) {
                for (y in area.y) {
                    val id = cloth.getOrPut(x to y) { area.id }
                    if (id != area.id) {
                        ids.remove(id)
                        ids.remove(area.id)
                    }
                }
            }
        }
        return ids.firstOrNull()
    }

    private data class Area(val id: Int, val x: IntProgression, val y: IntProgression)

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
            return Area(id, x until x + w, y until y + h)
        }
    }
}
