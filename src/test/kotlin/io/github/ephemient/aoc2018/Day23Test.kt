package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day23Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(7, Day23(lines1).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(36, Day23(lines2).part2())
    }

    companion object {
        private val lines1 = """
            pos=<0,0,0>, r=4
            pos=<1,0,0>, r=1
            pos=<4,0,0>, r=3
            pos=<0,2,0>, r=1
            pos=<0,5,0>, r=3
            pos=<0,0,3>, r=1
            pos=<1,1,1>, r=1
            pos=<1,1,2>, r=1
            pos=<1,3,1>, r=1
            """.trimIndent().lines()

        private val lines2 = """
            pos=<10,12,12>, r=2
            pos=<12,14,12>, r=2
            pos=<16,12,12>, r=4
            pos=<14,14,14>, r=6
            pos=<50,50,50>, r=200
            pos=<10,10,10>, r=5
            """.trimIndent().lines()
    }
}
