package day13

import util.inputToString

fun closeList(s: String): String  {
    if (s.first() != '[') return s.first().toString()
    var bracketCount = 1
    var pos = 0
    while (bracketCount > 0) {
        pos += 1
        when (s[pos]) {
            '[' -> bracketCount += 1
            ']' -> bracketCount -= 1
            else -> Unit
        }
    }
    return s.substring(0, pos + 1)
}

// if ,: next
// if [: find end
fun packets(s: String): List<String> =
    when {
        s.isEmpty() -> listOf()
        s.first() == '[' -> closeList(s).let { listOf(it) + packets(s.substring(it.length)) }
        s.first().isDigit() ->
            if (s.substring(0, 2) == "10") listOf("10") + packets(s.substring(2))
            else listOf(s.first().toString()) + packets(s.substring(1))
        else -> packets(s.substring(1))
    }

fun main() {
    val input = inputToString("/2022/input/thirteen_test.txt")
        .split("\n\n")
        .map { it.split("\n").let { x -> Pair(x[0], x[1]) } }
    input.forEach { p ->
        packets(p.first.substring(1, p.first.length)).forEach { println(it) }
    }
}