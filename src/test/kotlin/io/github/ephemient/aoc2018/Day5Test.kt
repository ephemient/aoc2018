package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day5Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(10, Day5(lines).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(4, Day5(lines).part2())
    }

    companion object {
        val lines = listOf("dabAcCaCBAcCcaDA")
    }
}
