package day12

import util.inputToList

// keep track of visited nodes
// if node has already been visited, kill the path
/*
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
paths map / collection?
[(0, 1), (1, 0)]

map<int, int> = x to y == optimal lookup


 */

typealias Coord = Pair<Int, Int>
typealias Path = MutableMap<Int, Int>
typealias Graph = List<List<Int>>

val heights = ('a'..'z').toList()
fun isReachable(current: Char, next: Char): Boolean = try {
    heights[heights.indexOf(current) + 1] == next
} catch (e: Exception) { false }

fun Path.addStep(coord: Coord): Boolean =
    if (get(coord.first) == coord.second) false
    else {
        put(coord.first, coord.second)
        true
    }

fun Graph.getStep(pos: Coord): Coord? =
    try {
        this[pos.first][pos.second]
        pos
    } catch (e: Exception) {
        null
    }

fun Graph.get(pos: Coord) = this[pos.first][pos.second]

fun Graph.getSteps(pos: Coord): List<Coord> =
    listOfNotNull(
        getStep(pos.copy(pos.first, pos.second + 1)),
        getStep(pos.copy(pos.first, pos.second - 1)),
        getStep(pos.copy(pos.first - 1, pos.second)),
        getStep(pos.copy(pos.first + 1, pos.second))
    ).mapNotNull { stepPos ->
        if (isReachable(get(pos)))
    }


fun main() {

    val heightMap = inputToList("/2022/input/twelve_test.txt")
    val startingPos = heightMap.indexOfFirst { it.contains('S') }.let {
        Pair(it, heightMap[it].indexOfFirst { c -> c == 'S' })
    }
}
