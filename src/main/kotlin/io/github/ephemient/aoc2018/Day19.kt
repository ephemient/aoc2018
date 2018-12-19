package io.github.ephemient.aoc2018

class Day19(lines: List<String>) {
    private val ip: Int
    private val instructions: List<Instruction>

    init {
        ip = requireNotNull(IP_PATTERN.matchEntire(lines.first())?.groups?.get(1)?.value).toInt()
        instructions = lines.drop(1).map { line ->
            val (op, a, b, c) = requireNotNull(INSTRUCTION_PATTERN.matchEntire(line)).destructured
            Instruction(enumValueOf(op.toUpperCase()), a.toInt(), b.toInt(), c.toInt())
        }
    }

    fun part1(): Int {
        val regs = IntArray(6)
        while (regs[ip] in instructions.indices) step(regs)
        return regs[0]
    }

    fun part2(): Int {
        val regs = IntArray(6)
        regs[0] = 1
        while (regs[ip] in instructions.indices) step(regs)
        return regs[0]
    }

    private fun sumFactors(n: Int): Int {
        var sum = 0
        var d = 1
        while (d * d < n) {
            if (n % d == 0) sum += d + n / d
            d++
        }
        if (d * d == n) sum += d
        return sum
    }

    private fun step(regs: IntArray) {
        run {
            val base = regs[ip]
            if (base + 14 !in instructions.indices) return@run
            val (_, _, _, i) = instructions[base]
                .takeIf { it.op == Op.SETI && it.a == 1 } ?: return@run
            if (i == ip) return@run
            val (_, _, _, j) = instructions[base + 1]
                .takeIf { it.op == Op.SETI && it.a == 1 } ?: return@run
            if (j == ip || j == i) return@run
            val (_, _, _, k) = instructions[base + 2]
                .takeIf { it.op == Op.MULR && it.a == i && it.b == j } ?: return@run
            if (k == ip || k == i || k == j) return@run
            val (_, _, n, _) = instructions[base + 3]
                .takeIf { it.op == Op.EQRR && it.a == k && it.c == k } ?: return@run
            if (n == ip || n == i || n == j || n == k) return@run
            val goal = regs[n].takeIf { it > 0 } ?: return@run
            val (_, _, _, _) = instructions[base + 4]
                .takeIf { it.op == Op.ADDR && it.a == k && it.b == ip && it.c == ip } ?: return@run
            val (_, _, _, _) = instructions[base + 5]
                .takeIf { it.op == Op.ADDI && it.a == ip && it.b == 1 && it.c == ip } ?: return@run
            val (_, _, _, m) = instructions[base + 6]
                .takeIf { it.op == Op.ADDR && it.a == i && it.b == 0 } ?: return@run
            if (m == ip || m == i || m == j || m == k || m == n) return@run
            val (_, _, _, _) = instructions[base + 7]
                .takeIf { it.op == Op.ADDI && it.a == j && it.b == 1 && it.c == j } ?: return@run
            val (_, _, _, _) = instructions[base + 8]
                .takeIf { it.op == Op.GTRR && it.a == j && it.b == n && it.c == k } ?: return@run
            val (_, _, _, _) = instructions[base + 9]
                .takeIf { it.op == Op.ADDR && it.a == ip && it.b == k && it.c == ip } ?: return@run
            val (_, _, _, _) = instructions[base + 10]
                .takeIf { it.op == Op.SETI && it.a == base + 1 && it.c == ip } ?: return@run
            val (_, _, _, _) = instructions[base + 11]
                .takeIf { it.op == Op.ADDI && it.a == i && it.b == 1 && it.c == i } ?: return@run
            val (_, _, _, _) = instructions[base + 12]
                .takeIf { it.op == Op.GTRR && it.a == i && it.b == n && it.c == k } ?: return@run
            val (_, _, _, _) = instructions[base + 13]
                .takeIf { it.op == Op.ADDR && it.a == k && it.b == ip && it.c == ip } ?: return@run
            val (_, _, _, _) = instructions[base + 14]
                .takeIf { it.op == Op.SETI && it.a == base && it.c == ip } ?: return@run
            regs[ip] = base + 15
            regs[i] = goal + 1
            regs[j] = goal + 1
            regs[k] = 1
            regs[m] += sumFactors(goal)
            return
        }
        check(regs[ip] != 1)
        val instruction = instructions[regs[ip]]
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
