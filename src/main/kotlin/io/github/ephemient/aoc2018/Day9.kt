package io.github.ephemient.aoc2018

class Day9(lines: List<String>) {
    private val players: Int
    private val target: Int

    init {
        val (players, target) = requireNotNull(PATTERN.matchEntire(lines.single())).destructured
        this.players = players.toInt()
        this.target = target.toInt()
    }

    fun part1(): Long? = play(players, target)

    fun part2(): Long? = play(players, 100 * target)

    private class IntRing(val capacity: Int, vararg elements: Int) {
        var size = elements.size.also { require(it <= capacity) }
            private set
        private var head = 0
        private val data = IntArray(capacity).also { elements.copyInto(it) }

        fun move(n: Int) {
            if (n < 0) {
                repeat(-n) {
                    val prev = Math.floorMod(head - 1, capacity)
                    val last = Math.floorMod(prev + size, capacity)
                    data[prev] = data[last]
                    head = prev
                }
            } else {
                repeat(n) {
                    val next = Math.floorMod(head + 1, capacity)
                    val end = Math.floorMod(head + size, capacity)
                    data[end] = data[head]
                    head = next
                }
            }
        }

        fun insert(element: Int) {
            check(size < capacity)
            val prev = Math.floorMod(head - 1, capacity)
            data[prev] = element
            size++
            head = prev
        }

        fun remove(): Int {
            check(size > 0)
            return data[head].also {
                head = Math.floorMod(head + 1, capacity)
                size--
            }
        }
    }

    companion object {
        private val PATTERN = """(\d+) players; last marble is worth (\d+) points""".toRegex()

        fun play(players: Int, target: Int): Long? {
            val scores = mutableMapOf<Int, Long>()
            val ring = IntRing(target, 0)
            for (n in 1..target) {
                if (n % 23 == 0) {
                    ring.move(-7)
                    scores[n % players] = scores.getOrElse(n % players) { 0L } + ring.remove() + n
                } else {
                    ring.move(2)
                    ring.insert(n)
                }
            }
            return scores.values.max()
        }
    }
}
