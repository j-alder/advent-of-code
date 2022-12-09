package day08

import util.inputToList

typealias Grid = List<List<Int>>

fun buildGrid(): Grid =
    inputToList("/2022/input/eight.txt").mapNotNull { ln ->
        if (ln.isEmpty()) null
        else ln.toCharArray().map { h -> h.digitToInt() }
    }

fun visibleRight(x: Int, y: Int, grid: Grid, treeHeight: Int): Boolean {
    var p = y
    while (p < grid[x].size - 1) {
        p++
        if (grid[x][p] >= treeHeight) return false
    }
    return true
}

fun visibleLeft(x: Int, y: Int, grid: Grid, treeHeight: Int): Boolean {
    var p = y
    while (p > 0) {
        p--
        if (grid[x][p] >= treeHeight) return false
    }
    return true
}

fun visibleUp(x: Int, y: Int, grid: Grid, treeHeight: Int): Boolean {
    var p = x
    while (p > 0) {
        p--
        if (grid[p][y] >= treeHeight) return false
    }
    return true
}

fun visibleDown(x: Int, y: Int, grid: Grid, treeHeight: Int): Boolean {
    var p = x
    while (p < grid.size - 1) {
        p++
        if (grid[p][y] >= treeHeight) return false
    }
    return true
}

fun isVisible(x: Int, y: Int, grid: Grid): Boolean {
    if (x == 0 || x == grid.size - 1 || y == 0 || y == grid[0].size - 1) {
        return true
    }
    return visibleRight(x, y, grid, grid[x][y])
            || visibleLeft(x, y, grid, grid[x][y])
            || visibleUp(x, y, grid, grid[x][y])
            || visibleDown(x, y, grid, grid[x][y])
}

fun countLeft(x: Int, y: Int, grid: Grid): Int {
    var count = 0
    var p = y
    while (p > 0) {
        p--
        count++
        if (grid[x][p] >= grid[x][y]) break
    }
    return count
}

fun countRight(x: Int, y: Int, grid: Grid): Int {
    var count = 0
    var p = y
    while (p < grid[x].size - 1) {
        p++
        count++
        if (grid[x][p] >= grid[x][y]) break
    }
    return count
}

fun countUp(x: Int, y: Int, grid: Grid): Int {
    var count = 0
    var p = x
    while (p > 0) {
        p--
        count++
        if (grid[p][y] >= grid[x][y]) break
    }
    return count
}

fun countDown(x: Int, y: Int, grid: Grid): Int {
    var count = 0
    var p = x
    while (p < grid.size - 1) {
        p++
        count++
        if (grid[p][y] >= grid[x][y]) break
    }
    return count
}

fun scenicScore(x: Int, y: Int, grid: Grid): Int =
    listOf(
        countDown(x, y, grid),
        countUp(x, y, grid),
        countRight(x, y, grid),
        countLeft(x, y, grid)
    ).fold(1) { r, s ->
        if (s > 0) r * s
        else r
    }

fun main() {
    var totalVisible = 0
    var highestScenicScore = 0
    val grid = buildGrid()
    grid.forEachIndexed { x, row ->
        row.forEachIndexed { y, _ ->
            if (isVisible(x, y, grid)) totalVisible++
            scenicScore(x, y, grid).let {
                if (it > highestScenicScore) highestScenicScore = it
            }
        }
    }
    println("Answer 1: $totalVisible")
    println("Answer 2: $highestScenicScore")
}
