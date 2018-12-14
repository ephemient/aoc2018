package io.github.ephemient.aoc2018 {
  import scala.collection.JavaConverters._
  import scala.collection.mutable.ListBuffer

  class Day8s(lines: java.util.List[String]) {
    import Day8s._

    private val input: Seq[Int] = lines.asScala.head.split(' ').map(_.toInt)

    def part1(): Int = input match {
      case Node(node) => node.collect[Int] { (children, metadata) => children.sum + metadata.sum }
    }

    def part2(): Int = input match {
      case Node(node) => node.collect[Int] { (children, metadata) =>
        if (children.isEmpty) metadata.sum else metadata.flatMap { i: Int => children.lift(i - 1) }.sum
      }
    }
  }

  object Day8s {
    class Node(val children: Seq[Node], val metadata: Seq[Int]) {
      def collect[A](operation: (Seq[A], Seq[Int]) => A): A =
        operation(children.map(_.collect(operation)), metadata)
    }

    object Node {
      def unapply(source: Seq[Int]): Option[Node] = {
        for {
          (node, length) <- buildTree(source, 0)
          if length == source.size
        } yield node
      }

      def buildTree(source: Seq[Int], start: Int): Option[(Node, Int)] = {
        if (start + 2 > source.size) return None
        val n = source(start)
        val m = source(start + 1)
        var consumed = 2
        val children = ListBuffer.empty[Node]
        for (_ <- 1 to n) buildTree(source, start + consumed) match {
          case None => return None
          case Some((child, length)) => {
            children += child
            consumed += length
          }
        }
        val length = consumed + m
        if (start + length > source.size) return None
        return Some((new Node(children.toSeq, source.slice(start + consumed, start + length)), length))
      }
    }
  }
}
