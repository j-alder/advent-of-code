package day03

import util.forEachLine
import util.inputToList

const val inputPath = "/2022/input/three.txt"

val alpha = ('a'..'z').toList() + ('A'..'Z').toList()

fun Char.priority() = alpha.indexOf(this) + 1

fun findShared(s: String): Char? =
    s.chunked(s.length/2).let { chunks ->
        chunks[0].find { c ->
            chunks[1].contains(c)
        }
    }

fun soln1() {
    var total = 0
    forEachLine(inputPath) { line ->
        val c: Char? = findShared(line)
        total += c?.priority() ?: 0.also {
            println("no match found for input $line")
        }
    }
    println("Answer 1: $total")
}

fun findBadge(g: Triple<String, String, String>): Char? =
    g.first.find { c -> g.second.contains(c) && g.third.contains(c) }

fun soln2() {
    var badgeTotal = 0
    inputToList(inputPath).chunked(3).forEach { chunk ->
        val group = Triple(chunk[0], chunk[1], chunk[2])
        val c: Char? = findBadge(group)
        badgeTotal += c?.priority() ?: 0.also {
            println("no match found for input $group")
        }
    }
    println("Answer 2: $badgeTotal")
}

fun main() {
    soln1()
    soln2()
}
