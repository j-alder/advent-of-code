package day05

import util.forEachLine

val vowels = "aeiou"

fun containsVowels(str: String, n: Int) {
    var m = n
    str.forEach { c ->
        if (vowels.contains(c)) {
            m -= 1
        }
    }
}

fun main() {
    forEachLine("/2015/input/five.txt") { line ->

    }
}
