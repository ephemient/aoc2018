package io.github.ephemient.aoc2018

import kotlin.test.Test
import kotlin.test.assertEquals

class Day20Test {
    @Test
    fun `part 1 examples`() {
        assertEquals(3, Day20(listOf("^WNE$")).part1())
        assertEquals(10, Day20(listOf("^ENWWW(NEEE|SSE(EE|N))$")).part1())
        assertEquals(18, Day20(listOf("^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")).part1())
        assertEquals(23,
            Day20(listOf("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")).part1())
        assertEquals(31,
            Day20(listOf("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"))
                .part1())
    }
}
