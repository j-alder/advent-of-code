package day11

import util.inputToString
import java.math.BigInteger

data class Monkey(
    var items: ArrayDeque<BigInteger>,
    val operation: (BigInteger) -> BigInteger,
    val test: Int,
    val ifTrue: Int,
    val ifFalse: Int
) {
    private fun catch(i: BigInteger) = items.addLast(i)
    private fun yeet(i: BigInteger, m: Monkey) = m.catch(i)
    fun inspect(mkmap: Map<Int, Monkey>, w: (BigInteger) -> BigInteger): Int =
        items.size.also {
            while (items.size > 0) {
                val i = items.removeFirst()
                val worry = w(operation(i))
                yeet(worry, if (worry % BigInteger.valueOf(test.toLong()) == BigInteger.valueOf(0)) mkmap[ifTrue]!! else mkmap[ifFalse]!!)
            }
        }
}

fun getStartingItems(s: String): ArrayDeque<BigInteger> =
    ArrayDeque(
        s.substring(18)
            .split(", ")
            .map { BigInteger.valueOf(it.toLong()) }
    )

fun getOperation(s: String): (BigInteger) -> BigInteger =
    s.substring(23)
        .split(" ").let { opStr ->
            when (opStr[0]) {
            "+" -> { o -> o + (if (opStr[1] == "old") o else BigInteger.valueOf(opStr[1].toLong())) }
            "*" -> { o -> o * (if (opStr[1] == "old") o else BigInteger.valueOf(opStr[1].toLong())) }
            else -> { _ -> BigInteger.valueOf(0) }
        } }

fun getTest(s: String): Int = s.substring(21).toInt()

fun getMonkeys(): MutableMap<Int, Monkey> =
    inputToString("/2022/input/eleven_test.txt")
        .split("\n\n")
        .fold(mutableMapOf()) { result, monke ->
            monke.split("\n").let { lns ->
                result[lns[0][7].digitToInt()] =
                    Monkey(
                        items = getStartingItems(lns[1]),
                        operation = getOperation(lns[2]),
                        test = getTest(lns[3]),
                        ifTrue = lns[4][29].digitToInt(),
                        ifFalse = lns[5][30].digitToInt()
                    )
                result
            }
        }

fun MutableCollection<Long>.productOfHighestTwo() = sorted().takeLast(2).let { it[0] * it[1] }

fun partOne() {
    val m = getMonkeys()
    val inspectionCounts = m.keys.fold(mutableMapOf<Int, Long>()) { result, key ->
        result[key] = 0
        result
    }
    var i = 0
    while (i < 20) {
        m.keys.sorted().forEach {
            inspectionCounts[it] = inspectionCounts[it]!! + m[it]!!.inspect(m, fun (i: BigInteger) = i / BigInteger.valueOf(3L))
        }
        i++
    }
    println("Answer 1: ${inspectionCounts.values.productOfHighestTwo()}")
}

fun partTwo() {
    val m = getMonkeys()
    val inspectionCounts = m.keys.fold(mutableMapOf<Int, Long>()) { result, key ->
        result[key] = 0
        result
    }
    var i = 0
    while (i < 10_000) {
        m.keys.sorted().forEach {
            inspectionCounts[it] = inspectionCounts[it]!! + m[it]!!.inspect(m, fun (i: BigInteger) = i)
        }
        i++
    }
    inspectionCounts.forEach {
        println("${it.key} ${it.value}")
    }
    println(inspectionCounts.values.productOfHighestTwo())
//    println("Answer 2: ${inspectionCounts.values.sorted().takeLast(2).let { it[0] * it[1] }}")
}

fun main() {
//    partOne()
    partTwo()
}
