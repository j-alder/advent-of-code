package day07

import util.forEachLine

interface Node {
    val name: String
}

data class Dir(
    override val name: String,
    val children: MutableList<Node>,
    val parent: Node?,
) : Node

data class File(
    override val name: String,
    val parent: Node,
    val size: Int
) : Node

fun Node.addDir(name: String) = when (this) {
    is Dir ->
        if (children.none { it.name == name }) {
            children.add(
                Dir(
                    name,
                    mutableListOf(),
                    this,
                )
            )
        } else false

    else -> throw Exception("addDir: Node is not a directory")
}

fun Node.addFile(name: String, size: Int) = when (this) {
    is Dir ->
        if (children.none { it.name == name }) {
            children.add(
                File(
                    name,
                    this,
                    size
                )
            )
        } else false

    else -> throw Exception("addFile: Node is not a directory")
}

fun Node.getChild(name: String) = when (this) {
    is Dir -> children.find {
        it.name == name
    } ?: throw Exception("No child with name $name found")

    else -> throw Exception("getChild: Node is not a directory")
}

fun Node.getParent() = when (this) {
    is Dir -> this.parent ?: throw Exception("Directory has no parents")
    is File -> this.parent
    else -> throw Exception("Node has no parents")
}

fun buildTree(): Dir {
    val rootNode = Dir("/", mutableListOf(), null)
    var currentNode = rootNode as Node
    forEachLine("/2022/input/seven.txt") { ln ->
        when {
            ln.startsWith("$ cd /") || ln.startsWith("$ ls")
                    || ln.isEmpty() -> Unit

            ln.startsWith("$ cd ..") ->
                currentNode = currentNode.getParent()

            ln.startsWith("$ cd") ->
                currentNode = currentNode.getChild(ln.split(" ")[2])

            ln.startsWith("dir") ->
                currentNode.addDir(ln.split(" ").last())

            else ->
                ln.split(" ").let { sizeName ->
                    currentNode.addFile(sizeName[1], sizeName[0].toInt())
                }
        }
    }
    return rootNode
}

fun getSize(n: Dir): Int =
    n.children.fold(0) { r, c ->
        when (c) {
            is File -> r + c.size
            is Dir -> getSize(c)
            else -> 0
        }
    }

fun buildSizes(): Map<String, Int> {
    val path = mutableListOf<String>()
    val dirSizes = mutableMapOf<String, Int>()
    forEachLine("/2022/input/seven.txt") { ln ->
        when {
            ln.startsWith("$ ls")
                    || ln.isEmpty()
                    || ln.startsWith("dir") -> Unit

            ln.startsWith("$ cd ..") ->
                path.removeLast()

            ln.startsWith("$ cd") ->
                path.add(ln.split(" ")[2])

            else ->
                path.forEachIndexed { index, _ ->
                    path.take(index + 1).joinToString("").let { currDir ->
                        dirSizes[currDir] = dirSizes.getOrDefault(currDir, 0) + ln.split(" ")[0].toInt()
                    }
                }
        }
    }
    return dirSizes
}

fun main() {
    val dirSizes = buildSizes()
    val soln1 = dirSizes.values.filter { it <= 100_000 }.sum()
    println("Answer 1: $soln1")

    val totalSpace = 70_000_000
    val targetSpace = 30_000_000
    val used = dirSizes.values.first()
    val free = totalSpace - used
    val target = targetSpace - free
    val soln2 = dirSizes.values.filter { it > target }.min()
    println("Answer 2: $soln2")
}
