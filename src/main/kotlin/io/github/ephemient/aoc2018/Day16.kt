package io.github.ephemient.aoc2018

class Day16(lines: List<String>) {
    private val samples: List<Sample>
    private val instructions: List<IntArray>

    init {
        val samples = mutableListOf<Sample>()
        val instructions = mutableListOf<IntArray>()
        for (group in lines.splitToSequence("")) {
            run {
                if (group.size != 3) return@run null
                val (before) = BEFORE_RE.matchEntire(group[0])?.destructured ?: return@run null
                val data = DATA_RE.matchEntire(group[1])?.value ?: return@run null
                val (after) = AFTER_RE.matchEntire(group[2])?.destructured ?: return@run null
                val (op, a, b, c) = data.split(' ')
                samples.add(
                    Sample(
                        r0 = before.split(',').map { it.trim().toInt() }.toIntArray(),
                        r1 = after.split(',').map { it.trim().toInt() }.toIntArray(),
                        op = op.toInt(), a = a.toInt(), b = b.toInt(), c = c.toInt()
                    )
                )
            } ?: group.map { DATA_RE.matchEntire(it) }.requireNoNulls().forEach {
                instructions.add(it.value.split(' ').map { it.toInt() }.toIntArray())
            }
        }
        this.samples = samples
        this.instructions = instructions
    }

    fun part1(): Int = samples.count { it.validOps.size >= 3 }

    fun part2(): Int {
        val codings = mutableMapOf<Int, Op>()
        val pending = samples.associateByTo(mutableMapOf(), Sample::op, Sample::validOps)
        while (pending.isNotEmpty()) {
            val (key, ops) = pending.entries.first { it.value.size == 1 }
            pending.remove(key)
            codings[key] = ops.single().also { op -> pending.values.forEach { it.remove(op) } }
        }
        val regs = IntArray(4)
        for ((op, a, b, c) in instructions) checkNotNull(codings[op])(a, b, c, regs)
        return regs[0]
    }

    private enum class Op {
        ADDR {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = r0[a] + r0[b]
            }
        },
        ADDI {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = r0[a] + b
            }
        },
        MULR {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = r0[a] * r0[b]
            }
        },
        MULI {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = r0[a] * b
            }
        },
        BANR {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = r0[a] and r0[b]
            }
        },
        BANI {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = r0[a] and b
            }
        },
        BORR {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = r0[a] or r0[b]
            }
        },
        BORI {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = r0[a] or b
            }
        },
        SETR {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = r0[a]
            }
        },
        SETI {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = a
            }
        },
        GTIR {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = if (a > r0[b]) 1 else 0
            }
        },
        GTRI {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = if (r0[a] > b) 1 else 0
            }
        },
        GTRR {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = if (r0[a] > r0[b]) 1 else 0
            }
        },
        EQIR {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = if (a == r0[b]) 1 else 0
            }
        },
        EQRI {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = if (r0[a] == b) 1 else 0
            }
        },
        EQRR {
            override operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray) {
                r1[c] = if (r0[a] == r0[b]) 1 else 0
            }
        };

        abstract operator fun invoke(a: Int, b: Int, c: Int, r0: IntArray, r1: IntArray = r0)
    }

    private data class Sample(
        val r0: IntArray,
        val r1: IntArray,
        val op: Int,
        val a: Int,
        val b: Int,
        val c: Int
    ) {
        init {
            require(r0.size == r1.size)
        }

        val validOps: MutableSet<Op>
            get() {
                val tmp = IntArray(r0.size)
                return enumValues<Op>().filterTo(mutableSetOf<Op>()) {
                    r0.copyInto(tmp)
                    it(a, b, c, r0, tmp)
                    tmp.contentEquals(r1)
                }
            }
    }

    companion object {
        private val BEFORE_RE = """Before: \[(\d+(?:,\s*\d+)*)\]""".toRegex()
        private val DATA_RE = """\d+(?:\s+\d+)*""".toRegex()
        private val AFTER_RE = """After:  \[(\d+(?:,\s*\d+)*)\]""".toRegex()
    }
}
