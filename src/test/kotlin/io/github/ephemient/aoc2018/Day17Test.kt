package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day17Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(57, Day17(lines).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(29, Day17(lines).part2())
    }

    companion object {
        private val lines = """
            x=495, y=2..7
            y=7, x=495..501
            x=501, y=3..7
            x=498, y=2..4
            x=506, y=1..2
            x=498, y=10..13
            x=504, y=10..13
            y=13, x=498..504
            """.trimIndent().lines()
    }
}
