package io.github.ephemient.aoc2018

import kotlin.math.abs

class Day25(lines: List<String>) {
    private val points: List<Point> = lines.mapNotNull { it.toPointOrNull() }

    fun part1(): Int {
        val groups = mutableMapOf<Point, MutableSet<Point>>()
        for (p1 in points) {
            for (p2 in points) {
                if (manhattanDistance(p1, p2) > 3) continue
                val s1 = groups.getOrPut(p1) { mutableSetOf(p1) }
                val s2 = groups.getOrPut(p2) { mutableSetOf(p2) }
                if (s1 === s2) continue
                s1.addAll(s2)
                for (p in s2) groups[p] = s1
            }
        }
        return groups.values.toSet().size
    }

    private data class Point(val x: Int, val y: Int, val z: Int, val t: Int)

    companion object {
        private val LINE_PATTERN = """\s*(-?\d+),\s*(-?\d+),\s*(-?\d+),\s*(-?\d+)\s*""".toRegex()

        private fun manhattanDistance(p1: Point, p2: Point): Int =
            abs(p2.x - p1.x) + abs(p2.y - p1.y) + abs(p2.z - p1.z) + abs(p2.t - p1.t)

        private fun String.toPointOrNull(): Point? {
            val (x, y, z, t) = LINE_PATTERN.matchEntire(this)?.destructured ?: return null
            return Point(x.toInt(), y.toInt(), z.toInt(), t.toInt())
        }
    }
}
