package io.github.ephemient.aoc2018

class Day9(lines: List<String>) {
    private val players: Int
    private val target: Int

    init {
        val (players, target) = requireNotNull(PATTERN.matchEntire(lines.first())).destructured
        this.players = players.toInt()
        this.target = target.toInt()
    }

    fun part1(): Long? = play(players, target)

    fun part2(): Long? = play(players, 100 * target)

    private class Ring<T>(element: T) {
        private var root = Node(element)

        fun insert(element: T) {
            root = Node(element, root, root.right)
        }

        fun delete(): T {
            require(root.left != root && root.right != root)
            val element = root.element
            root.left.right = root.right
            root.right.left = root.left
            root = root.right
            return element
        }

        fun move(n: Int) {
            if (n < 0) repeat(-n) { root = root.left } else repeat(n) { root = root.right }
        }

        override fun toString(): String = generateSequence(root) { it.right.takeIf { it !== root } }
            .joinToString(prefix = "[", postfix = "]") { it.element.toString() }

        private class Node<T> {
            var element: T
            var left: Node<T>
            var right: Node<T>

            constructor(element: T) {
                this.element = element
                left = this
                right = this
            }

            constructor(element: T, left: Node<T>, right: Node<T>) {
                this.element = element
                this.left = left.also { it.right = this }
                this.right = right.also { it.left = this }
            }
        }
    }

    companion object {
        private val PATTERN = """(\d+) players; last marble is worth (\d+) points""".toRegex()

        fun play(players: Int, target: Int): Long? {
            val scores = mutableMapOf<Int, Long>()
            val ring = Ring(0)
            for (n in 1..target) {
                if (n % 23 == 0) {
                    ring.move(-7)
                    scores[n % players] = scores.getOrElse(n % players) { 0L } + ring.delete() + n
                } else {
                    ring.move(1)
                    ring.insert(n)
                }
            }
            return scores.values.max()
        }
    }
}
