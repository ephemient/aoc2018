package io.github.ephemient.aoc2018

import java.util.PriorityQueue
import java.util.SortedMap
import kotlin.math.abs
import kotlin.math.sign

class Day23(lines: List<String>) {
    private val bots: List<Bot> = lines.mapNotNull { it.toBotOrNull() }

    fun part1(): Int? {
        val r = bots.map { it.r }.max() ?: return null
        return bots.filter { it.r == r }.map { (x, y, z) ->
            bots.count { (t, u, v) -> abs(t - x) + abs(u - y) + abs(v - z) <= r }
        }.max()
    }

    fun part2(): Int? {
        var bestCount = 0
        val bestDistance = mutableListOf<Octa>()
        val queue =
            PriorityQueue(compareBy(Pair<Int, SortedMap<Octa, MutableSet<Int>>>::first).reversed())
        with(bots.withIndex().groupingBy { it.value.toOcta() }.foldTo(
            destination = sortedMapOf<Octa, MutableSet<Int>>(),
            initialValueSelector = { _, _ -> mutableSetOf() },
            operation = { _, acc, (i, _) -> acc.apply { add(i) } }
        )) { queue.add(values.flatMapTo(mutableSetOf()) { it }.size to this) }
        while (queue.isNotEmpty()) {
            val (count, units) = queue.poll()
            if (count < bestCount) break
            val key = units.lastKey()
            val n = units.getValue(key).toMutableSet()
            val rest = units.headMap(key).toSortedMap()
            val sub = sortedMapOf<Octa, MutableSet<Int>>()
            for ((key2, m) in units.headMap(key)) {
                val key3 = key intersect key2 ?: continue
                (if (key == key3) n else sub.getOrPut(key3) { mutableSetOf() }).addAll(m)
            }
            sub.values.forEach { it.addAll(n) }
            if (n.size > bestCount) {
                bestCount = n.size
                bestDistance.clear()
            }
            if (n.size == bestCount) bestDistance.add(key)
            queue.add(rest.values.flatMapTo(mutableSetOf()) { it }.size to rest)
            queue.add(sub.values.flatMapTo(mutableSetOf()) { it }.size to sub)
        }
        return bestDistance.map(Octa::distanceToOrigin).min()
    }

    private data class Bot(val x: Int, val y: Int, val z: Int, val r: Int) {
        fun toOcta(): Octa = Octa(
            min = Quad(x + y + z - r, x + y - z - r, x - y - z - r, x - y + z - r),
            max = Quad(x + y + z + r, x + y - z + r, x - y - z + r, x - y + z + r)
        )
    }

    private data class Quad(
        val first: Int,
        val second: Int,
        val third: Int,
        val fourth: Int
    ) : Comparable<Quad> {
        override fun compareTo(other: Quad): Int = comparator.compare(this, other)

        companion object {
            private val comparator = compareBy(Quad::first)
                .thenBy(Quad::second)
                .thenBy(Quad::third)
                .thenBy(Quad::fourth)
        }
    }

    private data class Octa(val min: Quad, val max: Quad) : Comparable<Octa> {
        override fun compareTo(other: Octa) = comparator.compare(this, other)

        infix fun intersect(other: Octa): Octa? {
            val min = Quad(
                maxOf(min.first, other.min.first),
                maxOf(min.second, other.min.second),
                maxOf(min.third, other.min.third),
                maxOf(min.fourth, other.min.fourth)
            )
            val max = Quad(
                minOf(max.first, other.max.first),
                minOf(max.second, other.max.second),
                minOf(max.third, other.max.third),
                minOf(max.fourth, other.max.fourth)
            )
            if (min.first > max.first ||
                min.second > max.second ||
                min.third > max.third ||
                min.fourth > max.fourth
            ) {
                return null
            }
            return Octa(min, max)
        }

        val distanceToOrigin by lazy {
            if (min.first < max.first &&
                min.second < max.second &&
                min.third < max.third &&
                min.fourth < max.fourth
            ) {
                listOfNotNull(
                    minOf(abs(min.first), abs(max.first))
                        .takeIf { min.first.sign * max.first.sign >= 0 },
                    minOf(abs(min.second), abs(max.second))
                        .takeIf { min.second.sign * max.second.sign >= 0 },
                    minOf(abs(min.third), abs(max.third))
                        .takeIf { min.third.sign * max.third.sign >= 0 },
                    minOf(abs(min.fourth), abs(max.fourth))
                        .takeIf { min.fourth.sign * max.fourth.sign >= 0 }
                ).max() ?: 0
            } else {
                requireNotNull(
                    (min.first..max.first).flatMap { first ->
                        (min.second + (first xor min.second and 1)..max.second step 2)
                            .flatMap { second ->
                                (min.third + (first xor min.third and 1)..max.third step 2)
                                    .mapNotNull { third ->
                                        val x = (first + third) / 2
                                        val y = (second - third) / 2
                                        val z = (first - second) / 2
                                        (abs(x) + abs(y) + abs(z)).takeIf {
                                            x - y + z in min.fourth..max.fourth
                                        }
                                    }
                            }
                    }.min()
                )
            }
        }

        companion object {
            private val comparator = compareBy(Octa::min).reversed().thenBy(Octa::max).reversed()
        }
    }

    companion object {
        private val LINE_PATTERN = """pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)""".toRegex()

        private fun String.toBotOrNull(): Bot? {
            val (x, y, z, r) = LINE_PATTERN.matchEntire(this)?.destructured ?: return null
            return Bot(
                x.toIntOrNull() ?: return null,
                y.toIntOrNull() ?: return null,
                z.toIntOrNull() ?: return null,
                r.toIntOrNull() ?: return null
            )
        }
    }
}
