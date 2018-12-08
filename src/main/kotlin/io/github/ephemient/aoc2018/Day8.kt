package io.github.ephemient.aoc2018

class Day8(lines: List<String>) {
    private val input: IntArray = lines.first()
        .split(' ')
        .mapNotNull { it.toIntOrNull() }
        .toIntArray()

    private fun IntIterator.evaluate(f: (IntArray, IntArray) -> Int): Int {
        val n = next()
        val m = next()
        val a = IntArray(n) { evaluate(f) }
        val b = IntArray(m) { next() }
        return f(a, b)
    }

    fun part1(): Int = with(input.iterator()) {
        evaluate { a, b -> a.sum() + b.sum() }.also { check(!hasNext()) }
    }

    fun part2(): Int = with(input.iterator()) {
        evaluate { a, b ->
            if (a.isEmpty()) b.sum() else b.sumBy { a.getOrElse(it - 1) { 0 } }
        }.also { check(!hasNext()) }
    }
}
