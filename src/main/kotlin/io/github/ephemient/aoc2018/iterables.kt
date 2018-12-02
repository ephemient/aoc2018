package io.github.ephemient.aoc2018

fun <T, R> Iterable<T>.accumulate(initial: R, operation: (acc: R, T) -> R): Iterable<R> {
    val self = this
    return object : Iterable<R> {
        override fun iterator(): Iterator<R> {
            return object : AbstractIterator<R>() {
                private val iter = self.iterator()
                private var hasStarted = false
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
