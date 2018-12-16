package io.github.ephemient.aoc2018

class Day13(lines: List<String>) {
    private val crossings: Map<IntPair, Crossing>
    private val initialCarts: Map<IntPair, Cart>

    init {
        val crossings = sortedMapOf<IntPair, Crossing>()
        val initialCarts = sortedMapOf<IntPair, Cart>()
        for ((y, line) in lines.withIndex()) {
            for ((x, c) in line.withIndex()) {
                when (c) {
                    '/' -> crossings[IntPair(y, x)] = Crossing.Reflect
                    '\\' -> crossings[IntPair(y, x)] = Crossing.RReflect
                    '+' -> crossings[IntPair(y, x)] = Crossing.Plus
                    '^' -> initialCarts[IntPair(y, x)] = Cart(Orientation.North, 0)
                    '<' -> initialCarts[IntPair(y, x)] = Cart(Orientation.West, 0)
                    'v' -> initialCarts[IntPair(y, x)] = Cart(Orientation.South, 0)
                    '>' -> initialCarts[IntPair(y, x)] = Cart(Orientation.East, 0)
                }
            }
        }
        this.crossings = crossings
        this.initialCarts = initialCarts
    }

    fun part1(): String {
        val (y, x) = sequence<IntPair> { run() }.first()
        return "$x,$y"
    }

    fun part2(): String {
        val (y, x) = sequence<IntPair> { run() }.last()
        return "$x,$y"
    }

    private suspend fun SequenceScope<IntPair>.run() {
        var carts = initialCarts.toSortedMap()
        while (carts.size > 1) {
            val queue = carts
            carts = sortedMapOf<IntPair, Cart>()
            while (queue.isNotEmpty()) {
                var (pos, cart) = with(queue.entries.iterator()) { next().also { remove() } }
                pos = pos.move(cart.orientation)
                if (pos in carts || pos in queue) {
                    yield(pos)
                    carts.remove(pos)
                    queue.remove(pos)
                    continue
                }
                cart = when (crossings[pos]) {
                    Crossing.Reflect -> cart.copy(orientation = cart.orientation.swapNESW)
                    Crossing.RReflect -> cart.copy(orientation = cart.orientation.swapNWSE)
                    Crossing.Plus -> cart.copy(
                        orientation = when (cart.crossings % 3) {
                            0 -> cart.orientation.left
                            2 -> cart.orientation.right
                            else -> cart.orientation
                        },
                        crossings = cart.crossings + 1
                    )
                    else -> cart
                }
                carts[pos] = cart
            }
        }
        carts.keys.singleOrNull()?.let { yield(it) }
    }

    private data class Cart(val orientation: Orientation, val crossings: Int)

    private enum class Orientation {
        North, West, South, East
    }

    private enum class Crossing {
        Reflect, RReflect, Plus
    }

    companion object {
        private val Orientation.left: Orientation
            get() = when (this) {
                Orientation.North -> Orientation.West
                Orientation.West -> Orientation.South
                Orientation.South -> Orientation.East
                Orientation.East -> Orientation.North
            }

        private val Orientation.right: Orientation
            get() = when (this) {
                Orientation.West -> Orientation.North
                Orientation.South -> Orientation.West
                Orientation.East -> Orientation.South
                Orientation.North -> Orientation.East
            }

        private val Orientation.swapNESW: Orientation
            get() = when (this) {
                Orientation.North -> Orientation.East
                Orientation.East -> Orientation.North
                Orientation.South -> Orientation.West
                Orientation.West -> Orientation.South
            }

        private val Orientation.swapNWSE: Orientation
            get() = when (this) {
                Orientation.North -> Orientation.West
                Orientation.West -> Orientation.North
                Orientation.South -> Orientation.East
                Orientation.East -> Orientation.South
            }

        private fun IntPair.move(orientation: Orientation) = IntPair(
            first = when (orientation) {
                Orientation.North -> first - 1
                Orientation.South -> first + 1
                else -> first
            },
            second = when (orientation) {
                Orientation.West -> second - 1
                Orientation.East -> second + 1
                else -> second
            }
        )
    }
}