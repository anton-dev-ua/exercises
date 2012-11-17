package Dijkstra

class Dijkstra {
  type VertexValue = (Int, String)


  def initVertexValues(G: Map[String, Map[String, Int]], s: String): Map[String, VertexValue] = {
    G.keys.map(m => m -> (Int.MaxValue, "")).toMap + (s -> (0, ""))
  }

  def calcVertexValues(A: Map[String, VertexValue], g: Map[String, Int], l: VertexValue): Map[String, VertexValue] = {
    A ++ g.map(w => (w._1 -> List((l._1 + w._2, l._2), A(w._1)).minBy(_._1))).toMap
  }


  def findShortestPath(G: Map[String, Map[String, Int]], s: String, t: String): Map[String, VertexValue] = {
    findShortestPath(G, initVertexValues(G, s), t)
  }


  def findShortestPath(G: Map[String, Map[String, Int]], A: Map[String, VertexValue], t: String): Map[String, VertexValue] = {
    val c = A.minBy(_._2)._1
    if (c == t) return A
    A ++ findShortestPath(G, calcVertexValues(A - c, G(c), A(c)), t)
  }


  def extractPath(A: Map[String, VertexValue], t: String): List[String] = {
     if (t == "") return Nil
     extractPath(A, A(t)._2) ::: List(t)
  }
}
