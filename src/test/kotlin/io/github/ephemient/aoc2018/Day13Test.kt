package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day13Test {
    @Test
    fun `part 1 examples`() {
        assertEquals("7,3", Day13("""
            /->-\        
            |   |  /----\
            | /-+--+-\  |
            | | |  | v  |
            \-+-/  \-+--/
              \------/   
            """.trimIndent().lines()).part1())
    }

    @Test
    fun `part 2 examples`() {
        assertEquals("6,4", Day13("""
            />-<\  
            |   |  
            | /<+-\
            | | | v
            \>+</ |
              |   ^
              \<->/
            """.trimIndent().lines()).part2())
    }
}
