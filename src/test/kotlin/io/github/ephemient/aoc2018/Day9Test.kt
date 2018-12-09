package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day9Test {
    @Test
    fun examples() {
        assertEquals(32L, Day9.play(9, 25))
        assertEquals(8317L, Day9.play(10, 1618))
        assertEquals(146373L, Day9.play(13, 7999))
        assertEquals(2764L, Day9.play(17, 1104))
        assertEquals(54718L, Day9.play(21, 6111))
        assertEquals(37305L, Day9.play(30, 5807))
    }
}
