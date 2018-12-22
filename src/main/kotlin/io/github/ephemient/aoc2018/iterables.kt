package io.github.ephemient.aoc2018

fun <T, R> Iterable<T>.accumulate(
    initial: R,
    skipFirst: Boolean = false,
    operation: (acc: R, T) -> R
): Iterable<R> {
    val self = this
    return object : Iterable<R> {
        override fun iterator(): Iterator<R> {
            return object : AbstractIterator<R>() {
                private val iter = self.iterator()
                private var hasStarted = skipFirst
                private var acc = initial
                override fun computeNext() {
                    when {
                        !hasStarted -> {
                            hasStarted = true
                            setNext(acc)
                        }
                        iter.hasNext() -> {
                            acc = operation(acc, iter.next())
                            setNext(acc)
                        }
                        else -> done()
                    }
                }
            }
        }
    }
}

fun <T> Iterable<T>.cycle(): Iterable<T> {
    val self = this
    return object : Iterable<T> {
        override fun iterator(): Iterator<T> {
            return object : AbstractIterator<T>() {
                private var iter: Iterator<T> = self.iterator()
                override fun computeNext() {
                    if (iter.hasNext()) {
                        setNext(iter.next())
                    } else {
                        iter = self.iterator()
                        if (iter.hasNext()) setNext(iter.next()) else done()
                    }
                }
            }
        }
    }
}

fun Iterable<Int>.ranges(): List<IntRange> {
    val ranges = mutableListOf<IntRange>()
    var lo: Int? = null
    var hi: Int? = null
    for (element in this) when {
        lo == null || hi == null -> {
            lo = element
            hi = element
        }
        element in lo - 1..hi + 1 -> {
            lo = minOf(lo, element)
            hi = maxOf(hi, element)
        }
        else -> {
            ranges.add(lo..hi)
            lo = element
            hi = element
        }
    }
    if (lo != null && hi != null) ranges.add(lo..hi)
    return ranges
}

fun <T> Iterable<T>.splitToSequence(vararg delimiters: T, limit: Int = 0) = sequence<List<T>> {
    val group = mutableListOf<T>()
    var count = 1
    for (element in this@splitToSequence) {
        if ((limit <= 0 || count < limit) && element in delimiters) {
            if (group.isNotEmpty()) yield(group.toList())
            group.clear()
            count++
        } else {
            group.add(element)
        }
    }
    if (group.isNotEmpty()) yield(group.toList())
}
