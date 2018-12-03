package io.github.ephemient.aoc2018

data class IntPair(
    val first: Int,
    val second: Int
) : Comparable<IntPair> {
    override operator fun compareTo(other: IntPair): Int = comparator.compare(this, other)

    companion object {
        private val comparator = compareBy(IntPair::first).thenBy(IntPair::second)
    }
}

data class IntPairRange(
    override val start: IntPair,
    override val endInclusive: IntPair
) : ClosedRange<IntPair>, AbstractList<IntPair>() {
    private val firstRange: IntRange
        get() = start.first..endInclusive.first

    private val secondRange: IntRange
        get() = start.second..endInclusive.second

    override operator fun contains(value: IntPair): Boolean =
        value.first in firstRange && value.second in secondRange

    override val size: Int
        get() = firstRange.count() * secondRange.count()

    override operator fun get(index: Int): IntPair {
        if (index !in indices) throw IndexOutOfBoundsException()
        val stride = secondRange.count()
        return IntPair(first = start.first + index / stride, second = start.second + index % stride)
    }

    override fun isEmpty(): Boolean = firstRange.isEmpty() || secondRange.isEmpty()

    override fun indexOf(element: IntPair): Int {
        if (element !in this) return -1
        return firstRange.indexOf(element.first) * secondRange.count() +
                secondRange.indexOf(element.second)
    }

    fun asSequence(): Sequence<IntPair> =
        firstRange.asSequence().flatMap { first ->
            secondRange.asSequence().map { second ->
                IntPair(first, second)
            }
        }

    override fun iterator(): Iterator<IntPair> = asSequence().iterator()

    override fun lastIndexOf(element: IntPair) = indexOf(element)
}

operator fun IntPair.rangeTo(that: IntPair) = IntPairRange(this, that)
