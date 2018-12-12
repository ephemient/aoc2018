package io.github.ephemient.aoc2018

class Day11(lines: List<String>, private val size: Int = 300) {
    private val table = Array(size + 1) { IntArray(size + 1) }.also { table ->
        val serial = lines.single().toInt()
        for ((y, x) in IntPair(1, 1)..IntPair(size, size)) {
            table[y][x] = table[y - 1][x] + table[y][x - 1] - table[y - 1][x - 1] +
                ((x + 10) * y + serial) * (x + 10) / 100 % 10 - 5
        }
    }

    private fun box(x: Int, y: Int, n: Int): Int =
        table[y + n][x + n] - table[y][x + n] - table[y + n][x] + table[y][x]

    private fun maxBox(n: Int): Pair<Triple<Int, Int, Int>, Int>? {
        val (y, x) = (IntPair(0, 0)..IntPair(size - n, size - n)).maxBy { (y, x) -> box(x, y, n) }
            ?: return null
        return Triple(x + 1, y + 1, n) to box(x, y, n)
    }

    fun part1(): String? = maxBox(3)?.first?.let { (x, y, _) -> "$x,$y" }

    fun part2(): String? = List(size) { maxBox(it + 1) }
        .maxWith(nullsFirst(compareBy { it.second }))
        ?.first
        ?.let { (x, y, n) -> "$x,$y,$n" }
}
