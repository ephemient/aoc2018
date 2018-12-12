package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day12Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(325, Day12(lines).part1())
    }

    companion object {
        private val lines = """
            initial state: #..#.#..##......###...###

            ...## => #
            ..#.. => #
            .#... => #
            .#.#. => #
            .#.## => #
            .##.. => #
            .#### => #
            #.#.# => #
            #.### => #
            ##.#. => #
            ##.## => #
            ###.. => #
            ###.# => #
            ####. => #
            """.trimIndent().lines()
    }
}
