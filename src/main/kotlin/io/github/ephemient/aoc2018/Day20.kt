package io.github.ephemient.aoc2018

import java.util.ArrayDeque

class Day20(lines: List<String>) {
    private val edges: Map<IntPair, Set<IntPair>>

    init {
        val edges = mutableMapOf<IntPair, MutableSet<IntPair>>()
        var stack = mutableListOf<IntPair>()
        var p = IntPair(0, 0)
        loop@for (c in lines.single()) {
            val q = when (c) {
                'N' -> IntPair(p.first, p.second - 1)
                'E' -> IntPair(p.first + 1, p.second)
                'S' -> IntPair(p.first, p.second + 1)
                'W' -> IntPair(p.first - 1, p.second)
                '(' -> { stack.add(p); continue@loop }
                '|' -> { p = stack.last(); continue@loop }
                ')' -> { p = stack.removeAt(stack.lastIndex); continue@loop }
                else -> continue@loop
            }
            edges.getOrPut(p) { mutableSetOf() }.add(q)
            edges.getOrPut(q) { mutableSetOf() }.add(p)
            p = q
        }
        require(stack.isEmpty())
        this.edges = edges
    }

    fun part1(): Int = bfs(IntPair(0, 0)).last().first

    fun part2(): Int = bfs(IntPair(0, 0)).count { (depth, _) -> depth >= 1000 }

    private fun bfs(start: IntPair): Sequence<Pair<Int, IntPair>> = sequence<Pair<Int, IntPair>> {
        val queue = ArrayDeque(listOf(0 to start))
        val seen = mutableSetOf<IntPair>()
        while (queue.isNotEmpty()) {
            val (depth, p) = queue.remove().also { yield(it) }
            edges[p]?.filter(seen::add)?.mapTo(queue) { depth + 1 to it }
        }
    }
}
