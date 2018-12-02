package io.github.ephemient.aoc2018

import kotlin.math.abs

class Day1(lines: List<String>) {
    private val input = lines.map { it.toInt() }

    fun part1(): Int = input.sum()

    fun part2(): Int? {
        val seen = mutableSetOf<Int>()
        val list = input.accumulate(0) { a, b -> a + b }
            .onEach {
                if (!seen.add(it)) {
                    return it
                }
            }
            .drop(1)
            .toList()
        val total = list.last()
        return list.withIndex()
            .groupBy { Math.floorMod(it.value, total) }
            .values
            .flatMap { group ->
                group.withIndex().flatMap { (n, ix) ->
                    group.drop(n + 1).map { jy ->
                        val (i, x) = ix
                        val (j, y) = jy
                        val startIndex: Int
                        val endValue: Int
                        if ((total < 0) == (x < y)) {
                            startIndex = j
                            endValue = x
                        } else {
                            startIndex = i
                            endValue = y
                        }
                        Result(abs(x - y), startIndex, endValue)
                    }
                }
            }
            .min()
            ?.endValue
    }

    private data class Result(
        val gap: Int,
        val startIndex: Int,
        val endValue: Int
    ) : Comparable<Result> {
        override fun compareTo(other: Result): Int =
            gap.compareTo(other.gap)?.takeIf { it != 0 } ?: startIndex.compareTo(other.startIndex)
    }
}
