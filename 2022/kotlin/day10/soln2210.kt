package day10

import util.forEachLine

typealias CycleMap = Map<Int, Pair<Int, Int>>

fun buildCycleMap(): CycleMap {
    var currentX: Int
    var cycleMap: CycleMap = mutableMapOf()
    var cycle = 0
    forEachLine("/2022/input/ten.txt") { ln ->
        currentX = if (cycleMap.isNotEmpty()) cycleMap.entries.last().value.second else 1
        ln.split(" ").let { inst ->
            when (inst[0]) {
                "noop" ->
                    cycleMap = cycleMap.plus(++cycle to Pair(currentX, currentX))
                "addx" -> {
                    cycleMap = cycleMap.plus(++cycle to Pair(currentX, currentX))
                    cycleMap = cycleMap.plus(++cycle to Pair(currentX, currentX + inst[1].toInt()))
                }
            }
        }
    }
    return cycleMap
}

fun partOneSolution(): Int {
    val cycleMap = buildCycleMap()
    var n = 20
    var v: Int
    var s: Int
    var t = 0
    while (n < 221) {
        v = cycleMap[n]?.first ?: 0
        s = v * n
        t += s
        n += 40
    }
    return t
}

fun getRow(subMap: List<Map.Entry<Int, Pair<Int, Int>>>): String =
    subMap.foldIndexed("") { pos, result, entry ->
        result + if (entry.value.second in (pos..pos+2))
            '#'
        else
            '.'
    }

fun partTwoSolution() =
    buildCycleMap().entries.chunked(40).fold("") { result, subMap ->
        result + getRow(subMap) + '\n'
    }

fun main() {
    println("Answer 1: ${partOneSolution()}")
    println("Answer 2:\n${partTwoSolution()}")
}
