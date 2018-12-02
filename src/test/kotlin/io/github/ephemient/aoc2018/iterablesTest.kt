package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class iterablesTest {
    @Test
    fun `accumulate should work on an infinite stream`() {
        val odds = generateSequence(1) { it + 2 }.asIterable()
        val squares = odds.accumulate(0) { a, b -> a + b }
        assertEquals((0..9).map { it * it }, squares.take(10))
    }

    @Test
    fun `cycle should repeat`() {
        assertEquals(List(10) { 0..9 }.flatten(), (0..9).cycle().take(100))
    }

    @Test
    fun `cycle should work on an empty iterable`() {
        assertEquals(emptyList(), emptyList<Int>().cycle().toList())
    }

    @Test
    fun `cycle should work on a mutating iterable`() {
        val iter = object : Iterable<Int> {
            private var start = 0
            override fun iterator(): Iterator<Int> {
                val iter = (start..9).iterator()
                return object : Iterator<Int> by iter {
                    override fun next() = iter.next().also { if (it == 9) start++ }
                }
            }
        }
        assertEquals(List(10) { it..9 }.flatten(), iter.cycle().toList())
    }
}
