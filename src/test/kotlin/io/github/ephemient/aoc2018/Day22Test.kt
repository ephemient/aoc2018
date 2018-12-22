package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day22Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(114, Day22(lines).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(45, Day22(lines).part2())
    }

    companion object {
        private val lines = """
            depth: 510
            target: 10,10
            """.trimIndent().lines()
    }
}
