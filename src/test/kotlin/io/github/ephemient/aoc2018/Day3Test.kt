package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day3Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(4, Day3(lines).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(3, Day3(lines).part2())
    }

    companion object {
        private val lines = """
            #1 @ 1,3: 4x4
            #2 @ 3,1: 4x4
            #3 @ 5,5: 2x2
            """.trimIndent().lines()
    }
}
