package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day25Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(2, Day25(lines1).part1())
        assertEquals(4, Day25(lines2).part1())
        assertEquals(3, Day25(lines3).part1())
        assertEquals(8, Day25(lines4).part1())
    }

    companion object {
        private val lines1 = """
             0,0,0,0
             3,0,0,0
             0,3,0,0
             0,0,3,0
             0,0,0,3
             0,0,0,6
             9,0,0,0
            12,0,0,0
            """.trimIndent().lines()

        private val lines2 = """
            -1,2,2,0
            0,0,2,-2
            0,0,0,-2
            -1,2,0,0
            -2,-2,-2,2
            3,0,2,-1
            -1,3,2,2
            -1,0,-1,0
            0,2,1,-2
            3,0,0,0
            """.trimIndent().lines()

        private val lines3 = """
            1,-1,0,1
            2,0,-1,0
            3,2,-1,0
            0,0,3,1
            0,0,-1,-1
            2,3,-2,0
            -2,2,0,0
            2,-2,0,-1
            1,-1,0,-1
            3,2,0,2
            """.trimIndent().lines()

        private val lines4 = """
            1,-1,-1,-2
            -2,-2,0,1
            0,2,1,3
            -2,3,-2,1
            0,2,3,-2
            -1,-1,1,-2
            0,-2,-1,0
            -2,2,3,-1
            1,2,2,0
            -1,-2,0,-2
            """.trimIndent().lines()
    }
}
