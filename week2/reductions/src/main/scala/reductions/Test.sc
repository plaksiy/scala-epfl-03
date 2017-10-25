import common._

def max(a: Float, b: Float): Float = if (a > b) a else b

sealed abstract class Tree {
  def maxPrevious: Float
}

case class Node(left: Tree, right: Tree) extends Tree {
  val maxPrevious = max(left.maxPrevious, right.maxPrevious)
}

case class Leaf(from: Int, until: Int, maxPrevious: Float) extends Tree

def upsweepSequential(inp: Array[Float], from: Int, until: Int): Float = {
  var i = from
  var maxA = 0f
  while (i < until) {
    maxA = max(inp(i) / i, maxA)
    i += 1
  }
  maxA
}

def upsweep(inp: Array[Float], from: Int, end: Int, threshold: Int): Tree = {
  if (end - from <= threshold)
    // TODO
    Leaf(from, end, upsweepSequential(inp, from, end))
  else {
    val mid = (from + end) / 2
    val (tL, tR) = parallel(upsweep(inp, from, mid, threshold),
                            upsweep(inp, mid, end, threshold))
    Node(tL, tR)
  }
}

def downsweepSequential(input: Array[Float], output: Array[Float],
                        startingAngle: Float, from: Int, until: Int): Unit = {
  var i: Int = if (from < 1) 1 else from
  var maxA: Float = startingAngle
  while (i < until) {
    maxA = max(input(i) / i, maxA)
    output(i) = maxA
    i += 1
  }
}


def downsweep(input: Array[Float], output: Array[Float], startingAngle: Float,
              tree: Tree): Unit = tree match {
  case Leaf(from, until, maxPrevious) =>
    downsweepSequential(input, output, startingAngle, from, until)
  case Node(l, r) => {
    parallel(
      downsweep(input, output, startingAngle, l),
      downsweep(input, output, max(startingAngle, l.maxPrevious), r))
  }
}

val startingMoney = 3
val m = 2

m <= (if (startingMoney == 3) 2 else startingMoney * 2 / 3)


