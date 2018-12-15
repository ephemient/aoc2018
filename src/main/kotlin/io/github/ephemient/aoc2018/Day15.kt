package io.github.ephemient.aoc2018

class Day15(lines: List<String>) {
//    private val yRange: IntProgression
//    private val xRange: IntProgression
    private val walls: Set<IntPair>
    private val initialUnits: Map<IntPair, Unit>

    init {
//        var maxY = -1
//        var maxX = -1
        val walls = mutableSetOf<IntPair>()
        val initialUnits = mutableMapOf<IntPair, Unit>()
        for ((y, line) in lines.withIndex()) {
            for ((x, c) in line.withIndex()) {
                when (c) {
                    '#' -> walls.add(IntPair(y, x))
                    'E' -> initialUnits[IntPair(y, x)] = Unit(Species.Elf, 200)
                    'G' -> initialUnits[IntPair(y, x)] = Unit(Species.Goblin, 200)
                }
//                maxX = maxOf(maxX, x)
            }
//            maxY = maxOf(maxY, y)
        }
//        yRange = 0..maxY
//        xRange = 0..maxX
        this.walls = walls
        this.initialUnits = initialUnits
    }

    fun part1(): Pair<Int, Int> {
        val (rounds, units) = sequence<Map<IntPair, Unit>> { run() }
            .withIndex()
//            .onEach { (n, units) ->
//                System.err.println(
//                    if (n == 0) "Initially:" else "After $n round${if (n != 1) "s" else ""}:"
//                )
//                System.err.println(units.toDebugString())
//                System.err.println()
//            }
            .last()
        return rounds - 1 to units.values.sumBy { it.hp }
    }

    fun part2(): Triple<Int, Int, Int> {
        for (magic in 3..200) {
            try {
                val (rounds, units) = sequence<Map<IntPair, Unit>> { run(magic) }
                    .withIndex()
//                    .onEach { (n, units) ->
//                        System.err.print(
//                            if (n == 0) "Initially:" else "After $n round${if (n != 1) "s" else ""}"
//                        )
//                        System.err.println(" with elf power $magic:")
//                        System.err.println(units.toDebugString())
//                        System.err.println()
//                    }
                    .last()
                return Triple(magic, rounds - 1, units.values.sumBy { it.hp })
            } catch (_: DesynchronizationException) {
//                System.err.println("Christmas is cancelled! Needs more magic...")
            }
        }
        error("literally unplayable")
    }

    private suspend fun SequenceScope<Map<IntPair, Unit>>.run(magic: Int? = null) {
        var units = initialUnits.entries.associateTo(sortedMapOf<IntPair, Unit>()) { (key, value) ->
            key to value.copy()
        }
        while (true) {
            yield(units)
            val queue = units
            units = sortedMapOf<IntPair, Unit>()
            while (queue.isNotEmpty()) {
                var (k, unit) = with(queue.entries.iterator()) { next().also { remove() } }
                val otherUnits = units + queue
                val wallsAndOtherUnits = walls + otherUnits.keys
                val enemies = otherUnits.filter { it.value.species != unit.species }
                if (enemies.isEmpty()) {
                    yield(otherUnits + (k to unit))
                    return
                }
                if (k.adjacencies.none { it in enemies }) {
                    val enemyRanges = enemies.keys
                        .flatMapTo(sortedSetOf<IntPair>()) { it.adjacencies }
                    k = k.nearest(wallsAndOtherUnits, enemyRanges)
                        ?.nearest(wallsAndOtherUnits, k.adjacencies)
                        ?: k
                }
                k.adjacencies.mapNotNull { target -> enemies[target]?.let { target to it } }
                    .minWith(compareBy<Pair<IntPair, Unit>> { it.second.hp }.thenBy { it.first })
                    ?.let { (target, enemy) ->
                        val power = magic?.takeIf { unit.species == Species.Elf } ?: 3
                        enemy.hp -= power
                        if (enemy.hp <= 0) {
                            if (magic != null && enemy.species == Species.Elf) {
                                throw DesynchronizationException()
                            }
                            units.remove(target)
                            queue.remove(target)
                        }
                    }
                units[k] = unit
            }
        }
    }

//    private fun Map<IntPair, Unit>.toDebugString(): String {
//        val unitsByLine = entries.groupBy(
//            keySelector = { it.key.first },
//            valueTransform = { it.value }
//        )
//        return yRange.joinToString(separator = "\n") { y ->
//            xRange.joinToString(separator = "") { x ->
//                val k = IntPair(y, x)
//                if (k in walls) "#" else get(k)?.let {
//                    when (it.species) {
//                        Species.Elf -> "E"
//                        Species.Goblin -> "G"
//                    }
//                } ?: "."
//            } + "  " + (unitsByLine[y]?.joinToString(separator = ", ") {
//                when (it.species) {
//                    Species.Elf -> "E"
//                    Species.Goblin -> "G"
//                } + "(${it.hp})"
//            } ?: "")
//        }
//    }

    private enum class Species {
        Elf, Goblin
    }

    private data class Unit(val species: Species, var hp: Int)

    private class DesynchronizationException(
        message: String? = null,
        cause: Throwable? = null
    ) : Exception(message, cause)

    companion object {
        private val IntPair.adjacencies: List<IntPair>
            get() = listOf(
                IntPair(first - 1, second),
                IntPair(first, second - 1),
                IntPair(first, second + 1),
                IntPair(first + 1, second)
            )

        private fun IntPair.nearest(walls: Iterable<IntPair>, goals: Iterable<IntPair>): IntPair? {
            val visited = walls.toSortedSet()
            var queue = sortedSetOf(this)
            while (queue.isNotEmpty()) {
                queue.intersect(goals).min()?.let { return it }
                visited.addAll(queue)
                queue = queue.flatMapTo(sortedSetOf<IntPair>()) { it.adjacencies }
                queue.removeAll(visited)
            }
            return null
        }
    }
}
