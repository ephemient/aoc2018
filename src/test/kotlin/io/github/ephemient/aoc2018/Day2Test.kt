package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day2Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(12, Day2("abcdef bababc abbcde abcccd aabcdd abcdee ababab".split(" ")).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals("fgij", Day2("abcde fghij klmno pqrst fguij axcye wvxyz".split(" ")).part2())
    }
}
