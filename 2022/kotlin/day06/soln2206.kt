package day06

import java.util.LinkedList
import java.util.Queue
import util.inputToString

fun isUnique(s: List<Char>): Boolean =
    s.fold(mutableMapOf<Char, Int>()) { r, c ->
        r[c] = ((r[c] ?: 0) + 1)
        r
    }.values.all { it == 1 }

fun findPacket(inputText: String, markerLength: Int): String? {
    val x: Queue<Char> = LinkedList()
    inputText.forEachIndexed { i, c ->
        x.add(c)
        if (x.size > markerLength) {
            x.remove()
        }
        if (x.size == markerLength && isUnique(x.toList())) {
            return inputText.substring(0..i)
        }
    }
    return null
}

fun main() {
    val inputText = inputToString("/2022/input/six.txt")
    println("Answer 1: ${findPacket(inputText, 4)?.length ?: -1}")
    println("Answer 2: ${findPacket(inputText, 14)?.length ?: -1}")
}
