package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day15Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(47 to 590, Day15(lines0).part1())
        assertEquals(37 to 982, Day15(lines1).part1())
        assertEquals(46 to 859, Day15(lines2).part1())
        assertEquals(35 to 793, Day15(lines3).part1())
        assertEquals(54 to 536, Day15(lines4).part1())
        assertEquals(20 to 937, Day15(lines5).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals(Triple(15, 29, 172), Day15(lines0).part2())
        assertEquals(Triple(4, 33, 948), Day15(lines2).part2())
        assertEquals(Triple(15, 37, 94), Day15(lines3).part2())
        assertEquals(Triple(12, 39, 166), Day15(lines4).part2())
        assertEquals(Triple(34, 30, 38), Day15(lines5).part2())
    }

    companion object {
        private val lines0 = """
            #######
            #.G...#
            #...EG#
            #.#.#G#
            #..G#E#
            #.....#
            #######
            """.trimIndent().lines()
        private val lines1 = """
            #######
            #G..#E#
            #E#E.E#
            #G.##.#
            #...#E#
            #...E.#
            #######
            """.trimIndent().lines()
        private val lines2 = """
            #######
            #E..EG#
            #.#G.E#
            #E.##E#
            #G..#.#
            #..E#.#
            #######
            """.trimIndent().lines()
        private val lines3 = """
            #######
            #E.G#.#
            #.#G..#
            #G.#.G#
            #G..#.#
            #...E.#
            #######
            """.trimIndent().lines()
        private val lines4 = """
            #######
            #.E...#
            #.#..G#
            #.###.#
            #E#G#G#
            #...#G#
            #######
            """.trimIndent().lines()
        private val lines5 = """
            #########
            #G......#
            #.E.#...#
            #..##..G#
            #...##..#
            #...#...#
            #.G...G.#
            #.....G.#
            #########
            """.trimIndent().lines()
    }
}
