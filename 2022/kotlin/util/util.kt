package util

import java.io.File
import java.io.InputStream
import kotlin.Exception

fun getFile(path: String) = File(System.getProperty("user.dir") + path)

/**
 * Run a function [f] on each line of text in an input file
 * located at [path]
 *
 * Example [path]: /2022/input/one.txt
 */
fun forEachLine(path: String, f: (String) -> Unit) = try {
    getFile(path).forEachLine { f(it) }
} catch (e: Exception) {
    e.printStackTrace()
}

/**
 * Run a function [f] on each line of text in an input file
 * located at [path] until [until] is satisfied.
 */
fun forEachLineUntil(
    path: String,
    f: (String) -> Unit,
    until: (String) -> Boolean
) = try {
    getFile(path).useLines { lines ->
        for (li in lines) {
            if (until(li)) break
            f(li)
        }
    }
} catch (e: Exception) {
    e.printStackTrace()
}

/**
 * List of all lines in an input file located at [path]
 *
 * Example [path]: /2022/input/one.txt
 */
fun inputToList(path: String): List<String> = try {
    getFile(path).readLines()
} catch (e: Exception) {
    e.printStackTrace()
    emptyList()
}

fun inputToString(path: String) =
    getFile(path).readText()
