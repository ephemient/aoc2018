package io.github.ephemient.aoc2018

class Day5(lines: List<String>) {
    private val input = lines.first()

    fun part1(): Int = input.react()

    fun part2(): Int? = ('a'..'z')
        .map { input.replace("$it", "", ignoreCase = true).react() }
        .min()

    companion object {
        private fun String.react(): Int {
            if (isEmpty()) return 0
            val stack = StringBuilder()
            for ((i, b) in withIndex()) {
                val a = stack.lastOrNull()
                if (a != null &&
                    a.isUpperCase() != b.isUpperCase() &&
                    a.toUpperCase() == b.toUpperCase()) {
                    stack.deleteCharAt(stack.lastIndex)
                } else {
                    stack.append(b)
                }
            }
            return stack.length
        }
    }
}
