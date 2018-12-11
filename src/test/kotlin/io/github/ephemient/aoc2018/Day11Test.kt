package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day11Test {
    @Test
    fun `part 1 examples`() {
        assertEquals("33,45", Day11(listOf("18")).part1())
        assertEquals("21,61", Day11(listOf("42")).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals("90,269,16", Day11(listOf("18")).part2())
        assertEquals("232,251,12", Day11(listOf("42")).part2())
    }
}
