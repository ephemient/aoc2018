package io.github.ephemient.aoc2018

class Day5(lines: List<String>) {
    private val input = lines.first()

    fun part1(): Int = input.react().length

    fun part2(): Int? = ('a'..'z')
        .map { input.replace("$it", "", ignoreCase = true).react().length }
        .min()

    companion object {
        private val PATTERN = ('a'..'z')
            .map { a ->
                val b = a.toUpperCase()
                "$a$b|$b$a"
            }
            .joinToString(separator = "|")
            .toRegex()

        fun String.react(): String =
            generateSequence(this) { it.replace(PATTERN, "") }
                .zipWithNext()
                .mapNotNull { (a, b) -> a.takeIf { it == b } }
                .first()
    }
}
