package io.github.ephemient.aoc2018

class Day11(lines: List<String>, private val size: Int = 300) {
    private val table = Array(size + 1) { IntArray(size + 1) }.also { table ->
        val serial = lines.single().toInt()
        for ((y, x) in IntPair(1, 1)..IntPair(size, size)) {
            table[y][x] = table[y - 1][x] + table[y][x - 1] - table[y - 1][x - 1] +
                ((x + 10) * y + serial) * (x + 10) / 100 % 10 - 5
        }
    }

    fun part1(): String {
        val (y, x) = (IntPair(0, 0)..IntPair(size - 3, size - 3)).maxBy { (y, x) ->
            table[y + 3][x + 3] - table[y][x + 3] - table[y + 3][x] + table[y][x]
        }!!
        return "${x + 1},${y + 1}"
    }

    fun part2(): String {
        var bestPoint: Triple<Int, Int, Int>? = null
        var maxValue: Int? = null
        for ((y, x) in IntPair(0, 0)..IntPair(size - 1, size - 1)) {
            for (n in 1 until size - maxOf(x, y)) {
                val value = table[y + n][x + n] - table[y][x + n] - table[y + n][x] + table[y][x]
                if (maxValue == null || maxValue < value) {
                    bestPoint = Triple(x, y, n)
                    maxValue = value
                }
            }
        }
        val (x, y, n) = bestPoint!!
        return "${x + 1},${y + 1},$n"
    }
}
