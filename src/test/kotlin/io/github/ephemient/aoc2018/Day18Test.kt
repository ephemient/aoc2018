package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day18Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(1147, Day18(lines).part1())
    }

    companion object {
        private val lines = """
            .#.#...|#.
            .....#|##|
            .|..|...#.
            ..|#.....#
            #.#|||#|#|
            ...#.||...
            .|....|...
            ||...#|.#|
            |.||||..|.
            ...#.|..|.
            """.trimIndent().lines()
    }
}
