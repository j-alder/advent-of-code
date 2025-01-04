package day05

import util.inputToList
import kotlin.math.min

typealias State = MutableMap<Int, ArrayDeque<Char>>

// technically cheating - I couldn't come up with a way to eloquently parse this input
fun buildInitialState(): State {
    return mutableMapOf(
        1 to ArrayDeque(listOf('L', 'N', 'W', 'T', 'D')),
        2 to ArrayDeque(listOf('C', 'P', 'H')),
        3 to ArrayDeque(listOf('W', 'P', 'H', 'N', 'D', 'G', 'M', 'J')),
        4 to ArrayDeque(listOf('C', 'W', 'S', 'N', 'T', 'Q', 'L')),
        5 to ArrayDeque(listOf('P', 'H', 'C', 'N')),
        6 to ArrayDeque(listOf('T', 'H', 'N', 'D', 'M', 'W', 'Q', 'B')),
        7 to ArrayDeque(listOf('M', 'B', 'R', 'J', 'G', 'S', 'L')),
        8 to ArrayDeque(listOf('Z', 'N', 'W', 'G', 'V', 'B', 'R', 'T')),
        9 to ArrayDeque(listOf('W', 'G', 'D', 'N', 'P', 'L')),
    )
}

fun move(state: State, n: Int, f: Int, t: Int): State {
    for (i in 1..n) {
        if (state[f] != null && state[f]!!.size > 0) {
            val x = state[f]!!.removeLast()
            state[t]?.add(x)
        }
    }
    return state
}

fun moveMulti(state: State, n: Int, f: Int, t: Int): State {
    val x: ArrayDeque<Char> = ArrayDeque()
    for (i in 1..min(state[f]!!.size, n)) {
        x.addFirst(state[f]!!.removeLast())
    }
    while (x.isNotEmpty()) {
        state[t]!!.addLast(x.removeFirst())
    }
    return state
}

fun buildInstructions(): List<List<Int>> =
    inputToList("/2022/input/five.txt")
        .filter { it.startsWith("move") }
        .map { it.split(' ')
            .let { m ->
                listOf(m[1].toInt(), m[3].toInt(), m[5].toInt())
            }
        }

fun main() {
    val instructions = buildInstructions()
    var state = buildInitialState()
    instructions.forEach { inst ->
        state = move(state, inst[0], inst[1], inst[2])
    }
    println("Answer 1: ${state.entries.map { it.value.last() }.joinToString("")}")
    var state2 = buildInitialState()
    instructions.forEach { inst ->
        state2 = moveMulti(state2, inst[0], inst[1], inst[2])
    }
    println("Answer 2: ${state2.entries.map { it.value.last() }.joinToString("")}")
}
