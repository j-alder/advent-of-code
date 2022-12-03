package util

import java.io.File
import kotlin.Exception

/**
 * Run a function [f] on each line of text in an input file
 * located at [path]
 *
 * Example [path]: /2022/input/one.txt
 */
fun forEachLine(path: String, f: (String) -> Unit) = try {
    File(System.getProperty("user.dir") + path)
        .forEachLine { f(it) }
} catch (e: Exception) {
    e.printStackTrace()
}

/**
 * List of all lines in an input file located at [path]
 *
 * Example [path]: /2022/input/one.txt
 */
fun inputToList(path: String): List<String> = try {
    File(System.getProperty("user.dir") + path)
        .readLines()
} catch (e: Exception) {
    e.printStackTrace()
    emptyList()
}
