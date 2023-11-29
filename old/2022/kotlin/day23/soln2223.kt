package day23

import util.Coord
import util.inputToList
import kotlin.math.max

typealias Positions = MutableMap<Coord, Coord>
typealias AdjacencyMap = Map<String, Coord>

const val dirs = "NSWE"
fun getDirectionList(round: Int) = (round..round+3).map { dirs[it % dirs.length] }

fun Coord.adjacent(): AdjacencyMap = mapOf(
    "N" to Coord(first - 1, second),
    "NW" to Coord(first - 1, second - 1),
    "NE" to Coord(first - 1, second + 1),
    "S" to Coord(first + 1, second),
    "SE" to Coord(first + 1, second + 1),
    "SW" to Coord(first + 1, second - 1),
    "W" to Coord(first, second - 1),
    "E" to Coord(first, second + 1),
)

fun Coord.isOccupied(p: Positions) = p[this] != null

fun Positions.clear(dirs: List<String>, adjacencyMap: AdjacencyMap): Boolean =
    dirs.all { adjacencyMap[it]?.isOccupied(this) == false }

fun Coord.prospect(p: Positions, r: Int): Coord {
    adjacent().let { adjacencyMap ->
        if (adjacencyMap.values.all { !isOccupied(p) }) {
            p[this] = this
        }
        getDirectionList(r).forEach { dir ->
            when (dir) {
                'N' -> listOf("NE", "NW", "N")
                'S' -> listOf("NE", "NW", "N")
                'W' -> listOf("NE", "NW", "N")
                'E' -> listOf("NE", "NW", "N")
                else -> listOf()
            }.let { dirs ->
                if (p.clear(dirs, adjacencyMap)) {
                    return adjacencyMap[dir.toString()] ?: this
                }
            }
        }
    }
    return this
}

fun Positions.prospectAll(r: Int): Positions {
    this.entries.forEach {
        this[it.key] = it.key.prospect(this, r)
    }
    return this
}

fun Positions.resolveConflict(): Positions {
    this.entries.forEach { ent ->
        this.entries.filter { it.key != ent.key && it.value == ent.value }.forEach { dupeEnt ->
            this[dupeEnt.key] = dupeEnt.key
        }
    }
    return this
}

fun Positions.moveAll(): Positions = entries.fold(mutableMapOf()) { r, ent ->
    if (this[ent.value] == null) {
        r[ent.value] = ent.value
    } else {
        r[ent.key] = ent.key
    }
    r
}

fun main() {
    val posMap = inputToList("/2022/input/twentythree_test.txt")
        .foldIndexed(mutableMapOf<Coord, Coord>()) { x, r, ln ->
            ln.mapIndexed { y, c ->
                if (c == '#') r[Coord(x, y)] = Coord(x, y)
            }
            r
        }
    (0..9).forEach {
        posMap
            .prospectAll(it)
            .resolveConflict()
            .moveAll()
    }
    println(posMap)
}
