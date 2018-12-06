package io.github.ephemient.aoc2018

import kotlin.math.abs

class Day6(lines: List<String>) {
    private val input: List<Pair<Int, Int>>
    private val minX: Int
    private val maxX: Int
    private val minY: Int
    private val maxY: Int

    init {
        var minX: Int? = null
        var maxX: Int? = null
        var minY: Int? = null
        var maxY: Int? = null
        this.input = lines.mapNotNull { line ->
            val (sx, sy) = line.split(',').takeIf { it.size == 2 } ?: return@mapNotNull null
            val x = sx.trim().toIntOrNull() ?: return@mapNotNull null
            val y = sy.trim().toIntOrNull() ?: return@mapNotNull null
            if (minX?.takeIf { it <= x } == null) minX = x
            if (maxX?.takeIf { it >= x } == null) maxX = x
            if (minY?.takeIf { it <= y } == null) minY = y
            if (maxY?.takeIf { it >= y } == null) maxY = y
            x to y
        }
        this.minX = requireNotNull(minX)
        this.maxX = requireNotNull(maxX)
        this.minY = requireNotNull(minY)
        this.maxY = requireNotNull(maxY)
    }

    fun part1(): Int? {
        val bounds = IntPair(minX, minY)..IntPair(maxX, maxY)
        val field = arrayOfNulls<Pair<Int?, Int>>(bounds.size)
        for ((n, p) in input.withIndex()) {
            var distance = 0
            var q = setOf(IntPair(p.first, p.second))
            while (q.isNotEmpty()) {
                q = q.flatMapTo(mutableSetOf<IntPair>()) { p ->
                    val i = bounds.indexOf(p)
                    when (field[i]?.second?.takeIf { it <= distance }) {
                        null -> {
                            field[i] = n to distance
                            p.neighbors(bounds)
                        }
                        distance -> {
                            field[i] = null to distance
                            emptyList()
                        }
                        else -> emptyList()
                    }
                }
                distance++
            }
        }
        val border = (minX..maxX).flatMap { listOf(IntPair(it, minY), IntPair(it, maxY)) } +
            (minY + 1..maxY - 1).flatMap { listOf(IntPair(minX, it), IntPair(maxX, it)) }
        val exclude = border.mapNotNullTo(mutableSetOf<Int>()) { field[bounds.indexOf(it)]?.first }
        return field
            .mapNotNull { it?.first?.takeIf { it !in exclude } }
            .groupingBy { it }
            .eachCount()
            .values
            .max()
    }

    fun part2(limit: Int = 10000): Int? {
        val (xs, ys) = input.unzip()
        val radius = limit / input.size
        val dxs = (minX - radius..maxX + radius).map { x -> xs.sumBy { abs(it - x) } }
        val dys = (minY - radius..maxY + radius).map { y -> ys.sumBy { abs(it - y) } }
        return dxs.sumBy { dx -> dys.count { dy -> dx + dy < limit } }
    }

    companion object {
        private fun IntPair.neighbors(bounds: IntPairRange): List<IntPair> = listOfNotNull(
            IntPair(first - 1, second).takeIf { it in bounds },
            IntPair(first, second + 1).takeIf { it in bounds },
            IntPair(first + 1, second).takeIf { it in bounds },
            IntPair(first, second - 1).takeIf { it in bounds }
        )
    }
}
