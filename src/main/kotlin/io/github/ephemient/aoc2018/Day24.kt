package io.github.ephemient.aoc2018

class Day24(lines: List<String>) {
    private val groups: List<Group>

    init {
        val groups = mutableListOf<Group>()
        var team: String? = null
        var id = 0
        lines.forEach { line ->
            TEAM_PATTERN.matchEntire(line)?.destructured?.let { (it) ->
                team = it
                return@forEach
            }
            GROUP_PATTERN.matchEntire(line)?.let { m ->
                groups.add(
                    Group(
                        id = id++,
                        team = requireNotNull(team),
                        units = requireNotNull(m.groups["units"]?.value).toInt(),
                        hp = requireNotNull(m.groups["hp"]?.value).toInt(),
                        weak = (m.groups["weak1"] ?: m.groups["weak2"] ?: m.groups["weak3"])
                            ?.value?.split(", ")?.toSet() ?: emptySet(),
                        immune = (m.groups["immune1"] ?: m.groups["immune2"] ?: m.groups["immune3"])
                            ?.value?.split(", ")?.toSet() ?: emptySet(),
                        power = requireNotNull(m.groups["power"]?.value).toInt(),
                        type = requireNotNull(m.groups["type"]?.value),
                        initiative = requireNotNull(m.groups["initiative"]?.value).toInt()
                    )
                )
                return@forEach
            }
            require(line.isEmpty())
        }
        this.groups = groups
    }

    fun part1(): Int = run(groups.map { it.copy() }).values.sum()

    fun part2(): Int {
        val target = "Immune System"
        var boost = 0
        while (true) {
            val results = run(
                groups.map {
                    if (it.team == target) it.copy(power = it.power + boost) else it.copy()
                }
            )
            if (results.keys.singleOrNull() == target) return results.values.single()
            boost++
        }
    }

    private fun run(groups: List<Group>): Map<String, Int> {
        val seen = mutableSetOf<List<Group>>()
        var groups = groups
        while (
            groups !in seen && groups.groupingBy { it.team }.eachCountTo(mutableMapOf()).size > 1
        ) {
            seen.add(groups)
            val targets = groups.associateByTo(mutableMapOf<Int, Group>()) { it.id }
            val attacks = groups
                .sortedWith(
                    compareBy<Group> { it.units * it.power }.thenBy { it.initiative }.reversed()
                )
                .mapNotNull { attacker ->
                    val target = targets.values
                        .filter { attacker.team != it.team && attacker.type !in it.immune }
                        .maxBy { target ->
                            val multiplier = if (attacker.type in target.weak) 2 else 1
                            IntTriple(
                                multiplier * attacker.units * attacker.power,
                                target.units * target.power,
                                target.initiative
                            )
                        } ?: return@mapNotNull null
                    targets.remove(target.id)
                    IntTriple(attacker.initiative, attacker.id, target.id)
                }
                .sortedWith(compareBy(IntTriple::first).reversed())
            val map = groups.associateByTo(sortedMapOf<Int, Group>()) { it.id }
            for ((_, attackerId, defenderId) in attacks) {
                val attacker = map[attackerId]?.takeIf { it.units > 0 } ?: continue
                val defender = map[defenderId]?.takeIf { it.units > 0 } ?: continue
                val multiplier = if (attacker.type in defender.weak) 2 else 1
                defender.units -= multiplier * attacker.units * attacker.power / defender.hp
            }
            groups = map.values.filter { it.units > 0 }
        }
        return groups.groupingBy { it.team }.fold(0) { acc, group -> acc + group.units }
    }

    private data class Group(
        val id: Int,
        val team: String,
        var units: Int,
        val hp: Int,
        val weak: Set<String>,
        val immune: Set<String>,
        val power: Int,
        val type: String,
        val initiative: Int
    )

    companion object {
        private val TEAM_PATTERN = """(\w+(?:\s+\w+)*):""".toRegex()
        private val GROUP_PATTERN = """
            (?<units>\d+) units each with (?<hp>\d+) hit points 
            (?:
            \(weak to (?<weak1>\w+(?:, \w+)*)\) |\(immune to (?<immune1>\w+(?:, \w+)*)\) |
            \(weak to (?<weak2>\w+(?:, \w+)*); immune to (?<immune2>\w+(?:, \w+)*)\) |
            \(immune to (?<immune3>\w+(?:, \w+)*); weak to (?<weak3>\w+(?:, \w+)*)\) 
            )?
            with an attack that does 
            (?<power>\d+) (?<type>\w+) damage at initiative (?<initiative>\d+)
            """.trimIndent().lines().joinToString(separator = "").toRegex()
    }
}
