package day01

import util.forEachLine

fun main() {
    var topCals = listOf(0, 0, 0)
    var total = 0
    forEachLine("/2022/input/one.txt") { line ->
        if (line.isNotEmpty()) {
            total += line.toInt()
        } else {
            when {
                total >= topCals[0] ->
                    topCals = listOf(total, topCals[0], topCals[1])

                total >= topCals[1] ->
                    topCals = listOf(topCals[0], total, topCals[1])

                total >= topCals[2] ->
                    topCals = listOf(topCals[0], topCals[1], total)
            }
            total = 0
        }
    }
    println("Answer 1: ${topCals[0]}")
    println("Answer 2: ${topCals.sum()}")
}
