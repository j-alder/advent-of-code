package day12

import util.Coord
import util.inputToList

typealias Grid = List<String>

data class PathNode(
    val coord: Coord,
    val parent: PathNode?
)

/** All possible heights in the grid */
val heights = ('a'..'z').toList()

/** Whether [m] is attainable from the current sea level [n] */
fun isReachable(n: Char?, m: Char?): Boolean = try {
    n == 'z' && m == 'E' ||
    heights.indexOf(n).let { heights[it] == m || heights[it + 1] == m }
} catch (e: Exception) { false }

/**
 * Retrieve the Char at this Grid's x/y coordinate
 * specified in [c], otherwise null
 */
fun Grid.getValueAt(c: Coord) = try {
    this[c.first][c.second].let { if (it == 'S') 'a' else it }
} catch (e: Exception) { null }

/**
 * Return [c] if it is within the bounds of the
 * grid, otherwise null
 */
fun Grid.getCoord(c: Coord): Coord? = try {
    this[c.first][c.second]
    c
} catch (e: Exception) { null }

/** Find all adjacent coordinates to [c] that can be moved to */
fun Grid.adjacentTo(c: Coord): List<Coord> =
    listOfNotNull(
        getCoord(Coord(c.first, c.second - 1)),
        getCoord(Coord(c.first, c.second + 1)),
        getCoord(Coord(c.first - 1, c.second)),
        getCoord(Coord(c.first + 1, c.second))
    ).filter {
        isReachable(getValueAt(c), getValueAt(it))
    }

fun List<Coord>.undiscovered(p: Set<Coord>) = filter { !p.contains(it) }

fun main() {
    val grid = inputToList("/2022/input/twelve.txt")
    val startingCoord = grid.indexOfFirst { str ->
        str.contains('S')
    }.let { x ->
        Coord(x, grid[x].indexOfFirst { it == 'S' })
    }
    // Part 1 - BFS approach
    val p = mutableSetOf(startingCoord)
    val q = ArrayDeque<PathNode>()
    q.addLast(PathNode(startingCoord, null))
    var curr: PathNode? = null
    while (q.isNotEmpty()) {
        curr = q.removeFirst()
        if (grid.getValueAt(curr.coord) == 'E') break
        grid.adjacentTo(curr.coord).undiscovered(p).forEach {
            p.add(it)
            q.addLast(PathNode(it, curr))
        }
    }
    var count = 0
    while (curr?.parent != null) {
        count++
        curr = curr.parent
    }
    println("Answer 1: $count")
}
