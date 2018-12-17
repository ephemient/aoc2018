package io.github.ephemient.aoc2018

class Day17(lines: List<String>) {
    private val initialUnits: Map<IntPair, Element>
    private val minY: Int
    private val maxY: Int
    private val minX: Int
    private val maxX: Int

    init {
        val initialUnits = mutableMapOf<IntPair, Element>()
        var minY: Int? = null
        var maxY: Int? = null
        var minX: Int? = null
        var maxX: Int? = null

        for (line in lines) {
            val (id, sMajor, sLo, sHi) = requireNotNull(LINE_PATTERN.matchEntire(line)).destructured
            val major = sMajor.toInt()
            val range = sLo.toInt()..sHi.toInt()
            when (id) {
                "x" -> {
                    for (y in range) initialUnits[IntPair(y, major)] = Element.Wall
                    if (minY?.takeIf { it <= range.first } == null) minY = range.first
                    if (maxY?.takeIf { it >= range.last } == null) maxY = range.last
                    if (minX?.takeIf { it < major } == null) minX = major - 1
                    if (maxX?.takeIf { it > major } == null) maxX = major + 1
                }
                "y" -> {
                    for (x in range) initialUnits[IntPair(major, x)] = Element.Wall
                    if (minY?.takeIf { it <= major } == null) minY = major
                    if (maxY?.takeIf { it >= major } == null) maxY = major
                    if (minX?.takeIf { it < range.first } == null) minX = range.first - 1
                    if (maxX?.takeIf { it > range.last } == null) maxX = range.last + 1
                }
                else -> require(false)
            }
        }

        this.initialUnits = initialUnits.toMap()
        this.minY = requireNotNull(minY)
        this.maxY = requireNotNull(maxY)
        this.minX = requireNotNull(minX)
        this.maxX = requireNotNull(maxX)
    }

    fun part1(): Int {
        val units = initialUnits.toMutableMap()
        flood(units)
        return units.values.count { it == Element.Stagnant || it == Element.Flowing }
    }

    fun part2(): Int {
        val units = initialUnits.toMutableMap()
        flood(units)
        return units.values.count { it == Element.Stagnant }
    }

    private fun debug(units: Map<IntPair, Element>): String {
        val header = (minX..maxX)
            .joinToString(separator = "", postfix = "\n") { if (it == startX) "+" else "." }
        return (minY..maxY).joinToString(separator = "\n", prefix = header) { y ->
            (minX..maxX).joinToString(separator = "") { x ->
                when (units[IntPair(y, x)]) {
                    Element.Wall -> "#"
                    Element.Stagnant -> "~"
                    Element.Flowing -> "|"
                    else -> "."
                }
            }
        }
    }

    private fun flood(
        units: MutableMap<IntPair, Element>,
        y: Int = minY,
        x0: Int = startX,
        x1: Int = x0
    ): Boolean = y <= maxY && (x0..x1)
        .filter { units[IntPair(y, it)].isSpace }
        .ranges()
        .map { range ->
            if (!flood(units, y + 1, range.first, range.last)) {
                for (x in range) units[IntPair(y, x)] = Element.Flowing
                return@map false
            }
            val lefts = (range.first - 1 downTo minX).takeWhile { units[IntPair(y, it)].isSpace }
            val l = lefts.firstOrNull { units[IntPair(y + 1, it)].isSpaceOrFlowing }
                ?: lefts.lastOrNull()
            val rights = (range.last + 1..maxX).takeWhile { units[IntPair(y, it)].isSpace }
            val r = rights.firstOrNull { units[IntPair(y + 1, it)].isSpaceOrFlowing }
                ?: rights.lastOrNull()
            for (x in range) units[IntPair(y, x)] = Element.Wall
            val blockedL = l == null || flood(units, y, l, range.first - 1)
            val blockedR = r == null || flood(units, y, range.last + 1, r)
            val blocked = blockedL && blockedR
            val elt = if (blocked) Element.Stagnant else Element.Flowing
            for (x in (l ?: range.first)..(r ?: range.last)) units[IntPair(y, x)] = elt
            blocked
        }
        .fold((x0..x1).all { units[IntPair(y, it)] != Element.Flowing }) { a, b -> a && b }

    private enum class Element {
        Space, Wall, Stagnant, Flowing
    }

    companion object {
        private val LINE_PATTERN = """([xy])=(\d+), (?!\1)[xy]=(\d+)..(\d+)""".toRegex()
        private const val startX = 500

        private val Element?.isSpace: Boolean
            get() = this == null || this == Element.Space

        private val Element?.isSpaceOrFlowing: Boolean
            get() = this == null || this == Element.Space || this == Element.Flowing
    }
}
