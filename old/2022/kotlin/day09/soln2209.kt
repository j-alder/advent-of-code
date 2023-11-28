package day09

import util.forEachLine
import kotlin.math.abs

typealias Coord = Pair<Int, Int>
typealias LocationMap = MutableMap<Coord, Int>

fun Coord.sequenceTo(d: String, m: Int): List<Coord> =
    when (d) {
        "R" -> (0..m).map { step -> Coord(first, second + step) }
        "L" -> (0..m).map { step -> Coord(first, second - step) }
        "D" -> (0..m).map { step -> Coord(first + step, second) }
        "U" -> (0..m).map { step -> Coord(first - step, second) }
        else -> listOf(this)
    }

fun withinRange(diff: Int) = abs(diff) < 2

fun adjacent(diff: Int) = abs(diff) == 1

fun taut(diff: Int) = abs(diff) == 2

/**
 * Determine if there is a Knight's shaped difference in position
 */
fun isKnight(xDiff: Int, yDiff: Int): Boolean =
    (adjacent(xDiff) && taut(yDiff)) || (adjacent(yDiff) && taut(xDiff))

fun Coord.diagonalBetween(c: Coord): Coord = when {
    c.first > first && c.second > second -> copy(first + 1, second + 1)
    c.first > first && c.second < second -> copy(first + 1, second - 1)
    c.first < first && c.second > second -> copy(first - 1, second + 1)
    c.first < first && c.second < second -> copy(first - 1, second - 1)
    else -> this
}

fun Coord.follow(c: Coord): Coord {
    val xDiff = c.first - this.first
    val yDiff = c.second - this.second
    return when {
        withinRange(xDiff) && withinRange(yDiff)
            -> this
        isKnight(xDiff, yDiff)
            -> this.diagonalBetween(c)
        (taut(xDiff))
            -> this.copy(first, second + (if (xDiff > 0) 1 else -1))
        (taut(yDiff))
            -> this.copy(first + (if (yDiff > 0) 1 else -1), second)
        else
            -> this
    }
}

fun LocationMap.mutateBySeq(s: List<Coord>, startingTailPos: Coord): Pair<LocationMap, Coord> {
    var tailPos = startingTailPos
    s.forEach { headPos ->
        tailPos.follow(headPos).let { newTailPos ->
            if (newTailPos != tailPos) {
                this[newTailPos] = (this[newTailPos] ?: 0) + 1
                tailPos = newTailPos
            }
        }
    }
    return Pair(this, tailPos)
}

fun main() {
    var locMap = mutableMapOf(Coord(0, 0) to 1)
    var headPos = Coord(0, 0)
    var tailPos = headPos
    forEachLine("/2022/input/nine.txt") { ln ->
        ln.split(" ").let { inst ->
            val hSeq = headPos.sequenceTo(inst[0], inst[1].toInt())
            headPos = hSeq.last()
            val n = locMap.mutateBySeq(hSeq, tailPos)
            locMap = n.first
            tailPos = n.second
            println(ln)
            println("h: $headPos")
            println("t: $tailPos")
            println()
        }
    }
    println("Answer 1: ${locMap.entries.size}")
}