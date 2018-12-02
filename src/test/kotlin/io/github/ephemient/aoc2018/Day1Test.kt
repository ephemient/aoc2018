package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day1Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(3, Day1("+1, -2, +3, +1".split(", ")).part1())
        assertEquals(3, Day1("+1, +1, +1".split(", ")).part1())
        assertEquals(0, Day1("+1, +1, -2".split(", ")).part1())
        assertEquals(-6, Day1("-1, -2, -3".split(", ")).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(2, Day1("+1, -2, +3, +1".split(", ")).part2())
        assertEquals(0, Day1("+1, -1".split(", ")).part2())
        assertEquals(10, Day1("+3, +3, +4, -2, -4".split(", ")).part2())
        assertEquals(5, Day1("-6, +3, +8, +5, -6".split(", ")).part2())
        assertEquals(14, Day1("+7, +7, -2, -7, -4".split(", ")).part2())
    }
}
