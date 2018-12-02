package io.github.ephemient.aoc2018

class Day1(lines: List<String>) {
    private val input = lines.map { it.toInt() }

    fun part1(): Int = input.sum()

    fun part2(): Int {
        val seen = mutableSetOf<Int>()
        return input.cycle().accumulate(0) { a, b -> a + b }.first { !seen.add(it) }
    }

    companion object {
        fun main(args: Array<String>) {
            val day1 = Day1(Day1::class.java.classLoader.getResourceAsStream("day1.txt").bufferedReader().readLines())
            println(day1.part1())
            println(day1.part2())
        }
    }
}
