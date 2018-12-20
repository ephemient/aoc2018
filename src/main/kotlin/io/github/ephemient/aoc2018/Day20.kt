package io.github.ephemient.aoc2018

import java.util.ArrayDeque

class Day20(lines: List<String>) {
    private val edges: Map<IntPair, Set<IntPair>> =
        mutableMapOf<IntPair, MutableSet<IntPair>>().apply {
            val closeStack = mutableListOf<MutableSet<IntPair>>()
            val openStack = mutableListOf<MutableSet<IntPair>>()
            var current = mutableSetOf(IntPair(0, 0))
            loop@for (c in lines.single().removePrefix("^").removeSuffix("$")) {
                val (dx, dy) = when (c) {
                    'N' -> north
                    'E' -> east
                    'S' -> south
                    'W' -> west
                    '(' -> {
                        closeStack.add(mutableSetOf())
                        openStack.add(current)
                        continue@loop
                    }
                    '|' -> {
                        closeStack.last().addAll(current)
                        current = openStack.last()
                        continue@loop
                    }
                    ')' -> {
                        closeStack.last().addAll(current)
                        current = closeStack.removeAt(closeStack.lastIndex)
                        openStack.removeAt(openStack.lastIndex)
                        continue@loop
                    }
                    else -> error("$c")
                }
                current = current.mapTo(mutableSetOf<IntPair>()) { p ->
                    val q = IntPair(p.first + dx, p.second + dy)
                    getOrPut(p) { mutableSetOf() }.add(q)
                    getOrPut(q) { mutableSetOf() }.add(p)
                    return@mapTo q
                }
            }
            require(closeStack.isEmpty() && openStack.isEmpty())
        }

    fun part1(): Int = bfs(IntPair(0, 0)).last()

    fun part2(): Int = bfs(IntPair(0, 0)).dropWhile { it < 1000 }.count()

    private fun bfs(vararg start: IntPair): Sequence<Int> = sequence {
        val queue = start.mapTo(ArrayDeque()) { 0 to it }
        val seen = mutableSetOf<IntPair>()
        while (queue.isNotEmpty()) {
            val (depth, p) = queue.remove()
            yield(depth)
            edges[p]?.filter(seen::add)?.mapTo(queue) { depth + 1 to it }
        }
    }

    companion object {
        private val north = IntPair(0, -1)
        private val east = IntPair(1, 0)
        private val south = IntPair(0, 1)
        private val west = IntPair(-1, 0)
    }
}
