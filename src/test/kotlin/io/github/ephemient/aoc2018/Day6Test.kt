package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day6Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(17, Day6(lines).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(16, Day6(lines).part2(limit = 32))
    }

    companion object {
        private val lines = """
            1, 1
            1, 6
            8, 3
            3, 4
            5, 5
            8, 9
            """.trimIndent().lines()
    }
}
