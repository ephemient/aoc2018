package io.github.ephemient.aoc2018

class Day7(lines: List<String>) {
    private val dependencies: Map<Char, Set<Char>> = lines
        .flatMap<String, Pair<Char, Char?>> {
            val (a, b) = PATTERN.matchEntire(it)?.destructured ?: return@flatMap emptyList()
            listOf(a[0] to null, b[0] to a[0])
        }
        .groupBy(Pair<Char, Char?>::first, Pair<Char, Char?>::second)
        .mapValues { it.value.filterNotNull().toSet() }

    private val mutableDependencies: MutableMap<Char, MutableSet<Char>>
        get() = dependencies.mapValuesTo(sortedMapOf()) { it.value.toMutableSet() }

    fun part1(): String {
        val deps = mutableDependencies
        val sb = StringBuilder(deps.size)
        while (deps.isNotEmpty()) {
            val (k) = checkNotNull(deps.removeFirst { _, value -> value.isEmpty() })
            deps.values.forEach { it.remove(k) }
            sb.append(k)
        }
        return sb.toString()
    }

    fun part2(cost: Int = 60, workers: Int = 5): Int {
        val deps = mutableDependencies
        val working = sortedSetOf(
            comparator = compareBy<Pair<Int, Char>> { it.first }.thenBy { it.second }
        )
        var time = 0
        do {
            working.removeFirst()?.let { (t, k) ->
                time = t
                deps.remove(k)
                deps.values.forEach { it.remove(k) }
            }
            do {
                val (k) = deps.removeFirst { _, value -> value.isEmpty() } ?: break
                working.add(time + cost + k.toInt() - 'A'.toInt() + 1 to k)
            } while (working.size < workers)
        } while (working.isNotEmpty())
        check(deps.isEmpty())
        return time
    }

    companion object {
        private val PATTERN = """Step (.) must be finished before step (.) can begin\.""".toRegex()
    }
}
