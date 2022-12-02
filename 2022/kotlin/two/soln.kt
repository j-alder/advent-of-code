package two

import util.forEachLine

fun getFirstScore(n: Char, m: Char): Int =
    when (m) {
        'X' -> 1
        'Y' -> 2
        'Z' -> 3
        else -> 0
    } + when ("$n$m") {
        "AX", "BY", "CZ" -> 3
        "AY", "BZ", "CX" -> 6
        else -> 0
    }

fun getSecondScore(n: Char, m: Char): Int =
    when ("$n$m") {
        "AX" -> getFirstScore('A', 'Z')
        "AY" -> getFirstScore('A', 'X')
        "AZ" -> getFirstScore('A', 'Y')
        "BX", "BY", "BZ" -> getFirstScore(n, m)
        "CX" -> getFirstScore('C', 'Y')
        "CY" -> getFirstScore('C', 'Z')
        "CZ" -> getFirstScore('C', 'X')
        else -> 0
    }

fun main() {
    var firstTotalScore = 0
    var secondTotalScore = 0
    forEachLine("/2022/input/two.txt") { line ->
        firstTotalScore += getFirstScore(line[0], line[2])
        secondTotalScore += getSecondScore(line[0], line[2])
    }
    println("Answer 1: $firstTotalScore")
    println("Answer 2: $secondTotalScore")
}
