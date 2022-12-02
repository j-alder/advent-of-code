package util

import java.io.BufferedReader
import java.io.File
import java.io.FileReader
import java.lang.Exception

/**
 * Run a function [f] on each line of text in an input file
 * located at [path]
 *
 * Example [path]: /2022/kotlin/one/input.txt
 */
fun forEachLine(path: String, f: (String) -> Unit) = try {
    File(System.getProperty("user.dir") + path)
        .let { file ->
            BufferedReader(FileReader(file)).use { reader ->
                reader.lines().forEach { f(it) }
            }
        }
} catch (e: Exception) {
    e.printStackTrace()
}
