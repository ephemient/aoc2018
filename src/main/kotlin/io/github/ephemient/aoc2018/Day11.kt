package io.github.ephemient.aoc2018

class Day11(lines: List<String>) {
    private val serial = lines.first().toInt()

    private fun f(x: Int, y: Int): Int = ((x + 11) * (y + 1) + serial) * (x + 11) / 100 % 10 - 5

    fun part1(size: Int = 300): String {
        require(size > 0)
        val a = Array(size) { y -> IntArray(size) { x -> f(x, y) } }
        val (y, x) = (IntPair(0, 0)..IntPair(size - 3, size - 3)).maxBy { (y, x) ->
            (IntPair(y, x)..IntPair(y + 2, x + 2)).sumBy { (y, x) -> a[y][x] }
        }!!
        return "${x + 1},${y + 1}"
    }

    fun part2(size: Int = 300): String {
        require(size > 0)
        val a = Array(size) { y -> IntArray(size) { x -> f(x, y) } }
        var best = IntPair(-1, -1)
        var bestN = -1
        var bestP = Int.MIN_VALUE
        val memo = Array(size) { IntArray(size) }
        for (n in 0 until size) {
            for (p in IntPair(0, 0)..IntPair(size - n - 1, size - n - 1)) {
                val (y, x) = p
                memo[y][x] +=
                    (y..y + n - 1).sumBy { a[it][x + n] } + (x..x + n).sumBy { a[y + n][it] }
                if (memo[y][x] > bestP) {
                    best = p
                    bestN = n
                    bestP = memo[y][x]
                }
            }
        }
        check(best.first >= 0 && best.second >= 0 && bestN >= 0)
        return "${best.second + 1},${best.first + 1},${bestN + 1}"
    }
}
