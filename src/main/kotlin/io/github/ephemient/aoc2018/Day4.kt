package io.github.ephemient.aoc2018

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import java.time.temporal.ChronoField
import java.time.temporal.ChronoUnit
import java.time.temporal.Temporal
import java.time.temporal.TemporalField

class Day4(lines: List<String>) {
    private val input: List<Pair<LocalDateTime, Event>> =
        lines.mapNotNull { it.toTimedEventOrNull() }.sortedWith(
            compareBy(Pair<LocalDateTime, Event>::first).thenBy(Pair<LocalDateTime, Event>::second)
        )

    private val schedule: List<Triple<Int, LocalDateTime, LocalDateTime>> =
        with(mutableListOf<Triple<Int, LocalDateTime, LocalDateTime>>()) {
            var owner: Int? = null
            var start: LocalDateTime? = null
            for ((ts, event) in input) {
                when (event) {
                    is Event.Owner -> {
                        check(start == null)
                        owner = event.owner
                    }
                    is Event.Start -> {
                        check(owner != null && start == null)
                        start = ts
                    }
                    is Event.End -> {
                        add(Triple(checkNotNull(owner), checkNotNull(start), ts))
                        start = null
                    }
                }
            }
            check(start == null)
            toList()
        }

    fun part1(): Long? {
        val (maxOwner) = schedule.groupingBy { it.first }
            .fold(0L) { acc, (_, t0, t1) ->
                acc + ChronoUnit.MINUTES.between(t0, t1)
            }
            .maxBy { it.value } ?: return null
        val minutes = mutableMapOf<Long, Long>()
        for ((owner, t0, t1) in schedule) {
            if (owner != maxOwner) continue
            ChronoField.MINUTE_OF_HOUR.countBetweenTo(t0, t1, minutes)
        }
        val (maxMinute) = minutes.maxBy { it.value } ?: return null
        return maxOwner * maxMinute
    }

    fun part2(): Long? {
        val ownerMinutes = mutableMapOf<Int, MutableMap<Long, Long>>()
        for ((owner, t0, t1) in schedule) {
            ChronoField.MINUTE_OF_HOUR.countBetweenTo(
                t0, t1, ownerMinutes.getOrPut(owner) { mutableMapOf() }
            )
        }
        val (maxOwner, maxMinute) = ownerMinutes
            .mapNotNull { (owner, minutes) ->
                val (minute, count) = minutes.maxBy { it.value } ?: return@mapNotNull null
                Triple(owner, minute, count)
            }
            .maxBy { it.third } ?: return null
        return maxOwner * maxMinute
    }

    private sealed class Event : Comparable<Event> {
        data class Owner(val owner: Int) : Event()
        object Start : Event()
        object End : Event()

        override fun compareTo(other: Event): Int = when {
            this is Owner && other is Owner -> owner.compareTo(other.owner)
            this is Start && other is Start -> 0
            this is End && other is End -> 0
            this is Start -> -1
            other is Start -> 1
            else -> 0
        }
    }

    companion object {
        private val PATTERN =
            """\[(.*)\] (?:Guard #(\d+) begins shift|falls asleep()|wakes up())""".toRegex()
        private val FORMAT = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

        private fun String.toTimedEventOrNull(): Pair<LocalDateTime, Event>? {
            val match = PATTERN.matchEntire(this) ?: return null
            val ts = try {
                LocalDateTime.parse(match.groups[1]?.value ?: return null, FORMAT)
            } catch (e: DateTimeParseException) {
                println("parse: $e")
                return null
            }
            match.groups[2]?.value?.toIntOrNull()?.let { return ts to Event.Owner(it) }
            match.groups[3]?.let { return ts to Event.Start }
            match.groups[4]?.let { return ts to Event.End }
            return null
        }

        private fun TemporalField.countBetweenTo(
            t0: Temporal,
            t1: Temporal,
            destination: MutableMap<Long, Long>
        ) {
            require(isSupportedBy(t0) && isSupportedBy(t1))
            val (minimum, maximum) = range().run { minimum to maximum }
            val span = maximum - minimum + 1
            val size = baseUnit.between(t0, t1)
            val base = size / span
            val start = t0[this]
            val end = (start + size - minimum) % span + minimum
            for (value in minimum..maximum) {
                destination[value] = destination.getOrElse(value) { 0 } + base + when {
                    start < end && start <= value && value < end -> 1
                    start >= end && (start <= value || value < end) -> 1
                    else -> 0
                }
            }
        }
    }
}
