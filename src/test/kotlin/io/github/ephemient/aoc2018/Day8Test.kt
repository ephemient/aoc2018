package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day8Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(138, Day8(lines).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(66, Day8(lines).part2())
    }

    companion object {
        private val lines = listOf("2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2")
    }
}
