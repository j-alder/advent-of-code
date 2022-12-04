package four

import util.forEachLine

fun encompasses(ab: List<Int>, cd: List<Int>) =
    (ab[0] <= cd[0] && ab[1] >= cd[1]) || (cd[0] <= ab[0] && cd[1] >= ab[1])

fun overlaps(ab: List<Int>, cd: List<Int>): Boolean =
    Pair((ab[0]..ab[1]).toList(), (cd[0]..cd[1]).toList()).let { ranges ->
        ranges.first.any { ranges.second.contains(it) } || ranges.second.any { ranges.first.contains(it) }
    }

fun main() {
    var count1 = 0
    var count2 = 0
    forEachLine("/2022/input/four.txt") { line ->
        line.split(',').let { pairStrings ->
            val ab = pairStrings[0].split('-').map { it.toInt() }
            val cd = pairStrings[1].split('-').map { it.toInt() }
            if (encompasses(ab, cd)) count1++
            if (overlaps(ab, cd)) count2++
        }
    }
    println("Answer 1: $count1")
    println("Answer 2: $count2")
}
