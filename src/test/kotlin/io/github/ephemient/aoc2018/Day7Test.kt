package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day7Test {
    @Test
    fun `part 1 examples`() {
        assertEquals("CABDFE", Day7(lines).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(15, Day7(lines).part2(cost = 0, workers = 2))
    }

    companion object {
        private val lines = """
            Step C must be finished before step A can begin.
            Step C must be finished before step F can begin.
            Step A must be finished before step B can begin.
            Step A must be finished before step D can begin.
            Step B must be finished before step E can begin.
            Step D must be finished before step E can begin.
            Step F must be finished before step E can begin.
            """.trimIndent().lines()
    }
}
