package io.github.ephemient.aoc2018

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.State

@State(Scope.Thread)
open class Day10Bench {
    private lateinit var lines: List<String>

    @Setup
    fun prepare() {
        lines = javaClass.classLoader.getResourceAsStream("day10.txt").bufferedReader().readLines()
    }

    @Benchmark
    fun bothParts(): Pair<String, Int> = Day10(lines).run { part1() to part2() }
}
