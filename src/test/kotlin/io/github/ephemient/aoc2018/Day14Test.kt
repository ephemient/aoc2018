package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day14Test {
    @Test
    fun `part 1 examples`() {
        assertEquals("5158916779", Day14(listOf("9")).part1())
        assertEquals("0124515891", Day14(listOf("5")).part1())
        assertEquals("9251071085", Day14(listOf("18")).part1())
        assertEquals("5941429882", Day14(listOf("2018")).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(9, Day14(listOf("51589")).part2())
        assertEquals(5, Day14(listOf("01245")).part2())
        assertEquals(18, Day14(listOf("92510")).part2())
        assertEquals(2018, Day14(listOf("59414")).part2())
    }
}
