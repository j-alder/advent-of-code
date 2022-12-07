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
            children.add(Dir(
                name,
                mutableListOf(),
                this,
            ))
        } else false
    else -> throw Exception("addDir: Node is not a directory")
}

fun Node.addFile(name: String, size: Int) = when (this) {
    is Dir ->
        if (children.none { it.name == name }) {
            children.add(File(
                name,
                this,
                size
            ))
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
    forEachLine("/2022/input/seven_test.txt") { ln ->
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
    n.children.fold(0) { r, c -> when (c) {
        is File -> r + c.size
        is Dir -> getSize(c)
        else -> 0
    }}

fun getSizes(n: Dir): List<Int> =
    n.children.fold(listOf()) { r, c -> when (c) {
        is Dir -> r + getSize(c) + getSizes(c)
        else -> r
    }}

fun main() {
    val tree = buildTree()
    println(getSizes(tree))
}
