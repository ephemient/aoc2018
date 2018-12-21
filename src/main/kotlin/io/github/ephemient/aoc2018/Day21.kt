package io.github.ephemient.aoc2018

class Day21(lines: List<String>) {
    private val ip: Int
    private val instructions: List<Instruction>

    init {
        ip = requireNotNull(IP_PATTERN.matchEntire(lines.first())?.groups?.get(1)?.value).toInt()
        instructions = lines.drop(1).map { line ->
            val (op, a, b, c) = requireNotNull(INSTRUCTION_PATTERN.matchEntire(line)).destructured
            Instruction(enumValueOf(op.toUpperCase()), a.toInt(), b.toInt(), c.toInt())
        }
    }

    fun part1(): Int = sequence<Int> {
        val regs = IntArray(6)
        while (true) step(regs)
    }.first()

    fun part2(): Int {
        val seen = mutableSetOf<Int>()
        return sequence<Int> {
            val regs = IntArray(6)
            while (true) step(regs)
        }.takeWhile(seen::add).last()
    }

    private suspend fun SequenceScope<Int>.step(regs: IntArray) {
        val instruction = instructions[regs[ip]]
        val (op, a, b, c) = instruction
        check(c != 0) { "writing value to register 0" }
        if (op == Op.EQRR && a == 0 && b != 0) {
            yield(regs[b])
            regs[c] = 0
            regs[ip]++
            return
        }
        if (op == Op.EQRR && a != 0 && b == 0) {
            yield(regs[a])
            regs[c] = 0
            regs[ip]++
            return
        }
        check(op == Op.SETI || a != 0) { "reading from register 0" }
        check(
            op !in listOf(Op.ADDR, Op.MULR, Op.BANR, Op.BORR, Op.GTIR, Op.GTRR, Op.EQIR, Op.EQRR) ||
            b != 0
        ) { "reading from register 0" }
        run {
            val base = regs[ip]
            if (base + 8 !in instructions.indices) return@run
            val (_, _, _, t) = instructions[base]
                .takeIf { it.op == Op.SETI && it.a == 0 } ?: return@run
            if (t == 0 || t == ip) return@run
            val (_, _, _, u) = instructions[base + 1]
                .takeIf { it.op == Op.ADDI && it.a == t && it.b == 1 } ?: return@run
            if (u == 0 || u == ip || u == t) return@run
            val (_, _, n, _) = instructions[base + 2]
                .takeIf { it.op == Op.MULI && it.a == u && it.b > 0 && it.c == u } ?: return@run
            val (_, _, r, _) = instructions[base + 3]
                .takeIf { it.op == Op.GTRR && it.a == u && it.b > 0 && it.c == u } ?: return@run
            if (r == 0 || r == ip || r == t || r == u) return@run
            val (_, _, _, _) = instructions[base + 4]
                .takeIf { it.op == Op.ADDR && it.a == u && it.b == ip && it.c == ip } ?: return@run
            val (_, _, _, _) = instructions[base + 5]
                .takeIf { it.op == Op.ADDI && it.a == ip && it.b == 1 && it.c == ip } ?: return@run
            val (_, _, _, _) = instructions[base + 6].takeIf {
                it.op == Op.SETI && it.a == base + 8 && it.c == ip
            } ?: return@run
            val (_, _, _, _) = instructions[base + 7]
                .takeIf { it.op == Op.ADDI && it.a == t && it.b == 1 && it.c == t } ?: return@run
            val (_, _, _, _) = instructions[base + 8]
                .takeIf { it.op == Op.SETI && it.a == base && it.c == ip } ?: return@run
            regs[ip] = base + 9
            regs[t] = maxOf(0, regs[r] / n)
            regs[u] = 1
            return
        }
        regs[instruction.c] = instruction.op(regs, instruction.a, instruction.b)
        regs[ip]++
    }

    private enum class Op {
        ADDR { override operator fun invoke(r: IntArray, a: Int, b: Int): Int = r[a] + r[b] },
        ADDI { override operator fun invoke(r: IntArray, a: Int, b: Int): Int = r[a] + b },
        MULR { override operator fun invoke(r: IntArray, a: Int, b: Int): Int = r[a] * r[b] },
        MULI { override operator fun invoke(r: IntArray, a: Int, b: Int): Int = r[a] * b },
        BANR { override operator fun invoke(r: IntArray, a: Int, b: Int): Int = r[a] and r[b] },
        BANI { override operator fun invoke(r: IntArray, a: Int, b: Int): Int = r[a] and b },
        BORR { override operator fun invoke(r: IntArray, a: Int, b: Int): Int = r[a] or r[b] },
        BORI { override operator fun invoke(r: IntArray, a: Int, b: Int): Int = r[a] or b },
        SETR { override operator fun invoke(r: IntArray, a: Int, b: Int): Int = r[a] },
        SETI { override operator fun invoke(r: IntArray, a: Int, b: Int): Int = a },
        GTIR {
            override operator fun invoke(r: IntArray, a: Int, b: Int): Int = if (a > r[b]) 1 else 0
        },
        GTRI {
            override operator fun invoke(r: IntArray, a: Int, b: Int): Int = if (r[a] > b) 1 else 0
        },
        GTRR {
            override operator fun invoke(r: IntArray, a: Int, b: Int): Int =
                if (r[a] > r[b]) 1 else 0
        },
        EQIR {
            override operator fun invoke(r: IntArray, a: Int, b: Int): Int = if (a == r[b]) 1 else 0
        },
        EQRI {
            override operator fun invoke(r: IntArray, a: Int, b: Int): Int = if (r[a] == b) 1 else 0
        },
        EQRR {
            override operator fun invoke(r: IntArray, a: Int, b: Int): Int =
                if (r[a] == r[b]) 1 else 0
        };

        abstract operator fun invoke(r: IntArray, a: Int, b: Int): Int
    }

    private data class Instruction(val op: Op, val a: Int, val b: Int, val c: Int)

    companion object {
        private val IP_PATTERN = """#ip (\d+)""".toRegex()
        private val INSTRUCTION_PATTERN =
            """((?:add|mul|b(?:an|or)|set|(?:gt|eq)[ri])[ri]) (\d+) (\d+) (\d+)""".toRegex()
    }
}
