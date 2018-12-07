package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals
import kotlin.test.assertFalse
import kotlin.test.assertNotNull
import kotlin.test.assertNull

class collectionsTest {
    @Test
    fun `removeFirst on a MutableList`() {
        val list = mutableListOf(1, 2, 3, 4, 5)
        assertEquals(1, list.removeFirst())
        assertEquals(listOf(2, 3, 4, 5), list)
        assertNull(list.removeFirst { it < 2 })
        assertEquals(listOf(2, 3, 4, 5), list)
        assertEquals(3, list.removeFirst { it > 2 })
        assertEquals(listOf(2, 4, 5), list)
        list.clear()
        assertNull(list.removeFirst())
        assertEquals(emptyList<Int>(), list)
    }

    @Test
    fun `removeFirst on a MutableMap`() {
        val map = mutableMapOf('a' to 1, 'b' to 2, 'c' to 3, 'd' to 4)
        assertEquals('c' to 3, map.removeFirst { key, _ -> key == 'c' }?.toPair())
        assertEquals(mapOf('a' to 1, 'b' to 2, 'd' to 4), map)
        assertEquals('b' to 2, map.removeFirst { _, value -> value == 2 }?.toPair())
        assertEquals(mapOf('a' to 1, 'd' to 4), map)
        assertNull(map.removeFirst { key, value -> key < 'a' || value > 4 })
        assertEquals(mapOf('a' to 1, 'd' to 4), map)
        val (key, value) = assertNotNull(map.removeFirst())
        assertFalse(map.containsKey(key))
        assertFalse(map.containsValue(value))
        assertEquals(1, map.size)
    }
}
