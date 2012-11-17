package Dijkstra

import org.scalatest.Suite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DijkstraTestSuite extends Suite {

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

  def testInitialVertexMapShouldContainAllVertices() {
    val A = d.initVertexValues(G, "s")

    expect(Set("s", "v", "w", "t")) {
      A.keys
    }
  }

  def testInitialVertexMapShouldHaveMaxValueForAllVerticesExceptStartOne() {
    val A = d.initVertexValues(G, "s")

    assert(A("s")._1 == 0)
    assert(A("v")._1 == Int.MaxValue)
    assert(A("w")._1 == Int.MaxValue)
    assert(A("t")._1 == Int.MaxValue)
  }

  def testThatInitMethodSetsEmptyStepBackVertexForAllVertices() {
    val A = d.initVertexValues(G, "s")

    assert(A("s")._2 == "")
    assert(A("v")._2 == "")
    assert(A("w")._2 == "")
    assert(A("t")._2 == "")
  }


  def testThatCalcVertexValuesCalculatesNewValuesForAllNeighborVertices() {

    val A = Map("s" ->(0, ""), "v" ->(Int.MaxValue, ""), "w" ->(Int.MaxValue, ""), "t" ->(Int.MaxValue, ""))

    expect(Map("s" ->(0, ""), "v" ->(1, "s"), "w" ->(4, "s"), "t" ->(Int.MaxValue, ""))) {
      d.calcVertexValues(A, G("s"), (0, "s"))
    }

  }

  def testThatCalcVertexValuesDoesNotOverrideVertexValueIfNewlyCalculatedValueIsBigger() {

    val A = Map("v" ->(20, "d"), "w" ->(15, "k"))
    val g = Map("v" -> 7, "w" -> 10)

    expect((15, "k")) {
      d.calcVertexValues(A, g, (7, "s"))("w")
    }

  }

  def testThatExtractPathReturnsFoundShortestPath() {
    val B = Map("s" ->(0, ""), "v" ->(1, "s"), "w" ->(3, "v"), "t" ->(6, "w"))

    expect(List("s", "v", "w", "t")) {
      d.extractPath(B, "t")
    }

  }

  def testAlgorithmOnGraphWith4thVerticesAndShortestPath6() {
    expect(6) {
      (d.findShortestPath(G, "s", "t"))("t")._1
    }

  }

  def testAlgorithmOnGraphWith6thVerticesAndShortestPath20() {
    expect(20) {
      (d.findShortestPath(G2, "1", "5"))("5")._1
    }
  }

}
