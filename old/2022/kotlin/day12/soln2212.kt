package day12

import util.Coord
import util.inputToList
import java.util.PriorityQueue

typealias Grid = List<List<Int>>

data class PathNode(
    val coord: Coord,
    val steps: Int,
) : Comparable<PathNode> {
    override fun compareTo(other: PathNode) =
        steps.compareTo(other.steps)
}

val heights = ('a'..'z')
    .mapIndexed { idx, c -> c to idx }
    .toMap()
    .toMutableMap()
    .apply {
        this['E'] = 26
        this['S'] = 0
    }

fun Grid.getCoord(x: Int, y: Int): Coord? =
    try {
        this[x][y]
        Coord(x, y)
    } catch (e: Exception) {
        null
    }

fun Grid.adjacentTo(c: Coord) =
    listOfNotNull(
        getCoord(c.first, c.second + 1),
        getCoord(c.first, c.second - 1),
        getCoord(c.first + 1, c.second),
        getCoord(c.first - 1, c.second)
    )

fun List<Coord>.reachable(from: Coord, g: Grid): List<Coord> =
    filter { to -> g[to.first][to.second] - g[from.first][from.second] <= 1 }

fun List<Coord>.undiscovered(visited: Map<Coord, Boolean>) =
    filter { c -> visited[c] == false }

fun Grid.shortestPath(startingCoord: Coord, visited: MutableMap<Coord, Boolean>): Int {
    visited.keys.forEach { visited[it] = false }
    val queue = PriorityQueue<PathNode>()
    queue.add(PathNode(startingCoord, 0))
    var curr: PathNode?
    while (queue.isNotEmpty()) {
        curr = queue.poll()
        if (this[curr.coord.first][curr.coord.second] == 26) {
            return curr.steps
        }
        adjacentTo(curr.coord)
            .reachable(curr.coord, this)
            .undiscovered(visited)
            .forEach { c ->
                visited[c] = true
                queue.add(PathNode(c, curr.steps + 1))
            }
    }
    return Int.MAX_VALUE
}

fun Grid.shortestPathFrom(
    startingCoords: List<Coord>,
    visited: MutableMap<Coord, Boolean>
): Int = startingCoords.minOf { shortestPath(it, visited) }

fun main() {
    var startingCoord: Coord? = null
    val startingCoords = mutableListOf<Coord>()
    val visited = mutableMapOf<Coord, Boolean>()
    val grid: Grid = inputToList("/2022/input/twelve.txt")
        .mapIndexed { idx, ln ->
            ln.mapIndexed { idy, c ->
                if (c == 'S') startingCoord = Coord(idx, idy)
                if (c == 'S' || c == 'a') startingCoords.add(Coord(idx, idy))
                visited[Coord(idx, idy)] = false
                heights[c]!!
            }
        }
    println(startingCoords)
    val answer1 = grid.shortestPath(startingCoord!!, visited)
    val answer2 = grid.shortestPathFrom(startingCoords, visited)
    println("Answer 1: $answer1")
    println("Answer 2: $answer2")
}
