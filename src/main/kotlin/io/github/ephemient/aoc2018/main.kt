package io.github.ephemient.aoc2018

internal object Main

fun main(args: Array<String>) {
    val day1 = Day1(Main.javaClass.classLoader.getResourceAsStream("day1.txt").bufferedReader().readLines())
    println("Day 1")
    println(day1.part1())
    println(day1.part2())
    println()

    val day2 = Day2(Main.javaClass.classLoader.getResourceAsStream("day2.txt").bufferedReader().readLines())
    println("Day 1")
    println("Day 2")
    println(day2.part1())
    println(day2.part2())
    println()
}
