package day08

import util.inputToList

typealias Grid = List<List<Int>>

fun buildGrid(): Grid =
    inputToList("/2022/input/eight_test.txt").mapNotNull { ln ->
        if (ln.isEmpty()) null
        else ln.toCharArray().map { h -> h.digitToInt() }
    }

fun isVisible(x: Int, y: Int, grid: Grid): Boolean {
    if (x == 0 || x == grid.size - 1 || y == 0 || y == grid[0].size - 1) {
        return true
    }
    val treeHeight = grid[x][y]
    var xx = x
    var yy = y
    while (xx < grid.size - 1) {
        xx++
        if (grid[xx][yy] >= treeHeight) return false
    }
    xx = x
    while (xx > 0) {
        xx--
        if (grid[xx][yy] >= treeHeight) return false
    }
    xx = x
    while (yy < grid[xx].size - 1) {
        yy++
        if (grid[xx][yy] >= treeHeight) return false
    }
    yy = y
    while (yy > 0) {
        yy--
        if (grid[xx][yy] >= treeHeight) return false
    }
    return true
}

fun main() {
    var totalVisible = 0
    val grid = buildGrid()
    grid.forEachIndexed { rowIndex, row ->
        row.forEachIndexed { columnIndex, _ ->
            if (isVisible(rowIndex, columnIndex, grid)) totalVisible++
        }
    }
    println("Answer 1: $totalVisible")
}
