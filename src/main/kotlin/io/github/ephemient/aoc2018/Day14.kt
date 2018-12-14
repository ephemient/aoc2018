package io.github.ephemient.aoc2018

class Day14(lines: List<String>) {
    private val input: String = lines.single()

    fun part1(): String = game.drop(input.toInt()).take(10).joinToString(separator = "")

    fun part2(): Int? = game.windowed(input.length).indexOf(input.map(Character::getNumericValue))

    companion object {
        private val game: Sequence<Int> = sequence {
            var i = 0
            var j = 1
            val scores = mutableListOf(3, 7)
            yieldAll(scores)
            while (true) {
                val a = scores[i]
                val b = scores[j]
                val s = a + b
                if (s !in 0..9) {
                    scores.add(s / 10)
                    yield(s / 10)
                }
                scores.add(s % 10)
                yield(s % 10)
                i = (i + a + 1) % scores.size
                j = (j + b + 1) % scores.size
            }
        }
    }
}
