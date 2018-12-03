package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import kotlin.test.assertFalse
import kotlin.test.assertTrue

class rangesTest {
    @Test
    fun `comparison in row-major order`() {
        assertTrue(IntPair(0, 0) <= (IntPair(0, 0)))
        assertFalse(IntPair(0, 0) < (IntPair(0, 0)))
        assertTrue(IntPair(0, 0) < (IntPair(0, 1)))
        assertTrue(IntPair(0, 0) < (IntPair(1, 0)))
        assertFalse(IntPair(1, 0) < (IntPair(0, 1)))
    }

    @Test
    fun `can create empty IntPairRange`() {
        for (range in listOf(
                IntPair(1, 1)..IntPair(0, 0),
                IntPair(1, 0)..IntPair(0, 0),
                IntPair(0, 1)..IntPair(0, 0))) {
            assertEquals(0, range.size)
            assertTrue(range.isEmpty())
            assertFailsWith(IndexOutOfBoundsException::class) { range[0] }
        }
    }

    @Test
    fun `contains in IntPairRange`() {
        val range = IntPair(0, 0)..IntPair(3, 2)
        assertTrue(IntPair(0, 0) in range)
        assertTrue(IntPair(3, 0) in range)
        assertTrue(IntPair(0, 2) in range)
        assertTrue(IntPair(3, 2) in range)
        assertFalse(IntPair(3, 3) in range)
    }

    @Test
    fun `iteration in row-major order`() {
        val range = IntPair(0, 0)..IntPair(3, 2)
        assertEquals(12, (IntPair(0, 0)..IntPair(3, 2)).size)
        assertEquals(IntPair(0, 0), range[0])
        assertEquals(IntPair(3, 0), range[9])
        assertEquals(IntPair(0, 2), range[2])
        assertEquals(IntPair(3, 2), range[11])
        assertFailsWith(IndexOutOfBoundsException::class) { range[-1] }
        assertFailsWith(IndexOutOfBoundsException::class) { range[12] }
        assertEquals(0, range.indexOf(IntPair(0, 0)))
        assertEquals(9, range.indexOf(IntPair(3, 0)))
        assertEquals(2, range.indexOf(IntPair(0, 2)))
        assertEquals(11, range.indexOf(IntPair(3, 2)))
        assertEquals(-1, range.indexOf(IntPair(3, 3)))
        assertEquals(
            listOf(
                IntPair(0, 0), IntPair(0, 1), IntPair(0, 2),
                IntPair(1, 0), IntPair(1, 1), IntPair(1, 2),
                IntPair(2, 0), IntPair(2, 1), IntPair(2, 2),
                IntPair(3, 0), IntPair(3, 1), IntPair(3, 2)
            ),
            range.toList()
        )
    }
}
