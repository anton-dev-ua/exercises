package Dijkstra


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DijkstraTestFunSuite extends FunSuite with ShouldMatchers {

  val G: Map[String, Map[String, Int]] = Map(
    "s" -> Map("v" -> 1, "w" -> 4),
    "v" -> Map("w" -> 2, "t" -> 6),
    "w" -> Map("t" -> 3),
    "t" -> Map()
  )

  val G2: Map[String, Map[String, Int]] = Map(
    "1" -> Map("2" -> 7, "3" -> 9, "6" -> 14),
    "2" -> Map("3" -> 10, "4" -> 15),
    "3" -> Map("6" -> 2, "4" -> 11),
    "4" -> Map("5" -> 6),
    "5" -> Map(),
    "6" -> Map("5" -> 9)
  )

  val d: Dijkstra = new Dijkstra()


  test("initial vertex values map should contain all vertices") {
    d.initVertexValues(G, "s").keys should be(Set("s", "v", "w", "t"))
  }

  test("initial vertex values should be Max except start one") {
    d.initVertexValues(G, "s").filter(_._1 != "s").map(a => a._2._1) should be(List(Int.MaxValue, Int.MaxValue, Int.MaxValue))
  }

  test("initial start vertex value should be zero") {
    (d.initVertexValues(G, "s"))("s")._1 should be(0)
  }

  test("initial vertex's links to previous vertex of the path should by empty (path not found yet)") {
    d.initVertexValues(G, "s").map(a => a._2._2) should be(List("", "", "", ""))
  }

  test("calculation of vertex values should calculate new value for every neighbor vertex and this value should be the sum of current vertex value and distance to the neigbor vertex") {
    val A = Map("s" ->(0, ""), "v" ->(Int.MaxValue, ""), "w" ->(Int.MaxValue, ""), "t" ->(Int.MaxValue, ""))

    d.calcVertexValues(A, G("s"), (0, "s")).map(m => m._1 -> m._2._1) should be(Map("s" -> 0, "v" -> 1, "w" -> 4, "t" -> Int.MaxValue))
  }

  test("calculation of vertex values should store previous vertex in next vertex") {
    val A = Map("s" ->(0, ""), "v" ->(Int.MaxValue, ""), "w" ->(Int.MaxValue, ""), "t" ->(Int.MaxValue, ""))

    d.calcVertexValues(A, G("s"), (0, "s")).map(m => m._1 -> m._2._2) should be(Map("s" -> "", "v" -> "s", "w" -> "s", "t" -> ""))
  }

  test("calculation of vertex values should not override vertex value if new value is bigger") {
    val A = Map("v" ->(20, "d"), "w" ->(15, "k"))
    val g = Map("v" -> 7, "w" -> 10)

    d.calcVertexValues(A, g, (7, "s"))("w") should be((15, "k"))
  }

  test("extract path method should return list of vertices which represent found path from stat to finish vertex") {
    val B = Map("s" ->(0, ""), "v" ->(1, "s"), "w" ->(3, "v"), "t" ->(6, "w"))

    d.extractPath(B, "t") should be(List("s", "v", "w", "t"))
  }

  test("test algorithm on graph with 4th vertices and shortest path 6") {
    (d.findShortestPath(G, "s", "t"))("t")._1 should be(6)
  }

  test("test algorithm on graph with 6th vertices and shortest path 20") {
    (d.findShortestPath(G2, "1", "5"))("5")._1 should be(20)
  }

}