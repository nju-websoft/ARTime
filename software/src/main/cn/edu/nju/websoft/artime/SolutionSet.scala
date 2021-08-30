package cn.edu.nju.websoft.artime

import cn.edu.nju.websoft.artime.action.Action

import scala.collection.mutable

class SolutionSet(
  val solutions: mutable.Set[List[Action]]
) extends {
  val solutionHash: mutable.Set[String] = solutions.map(_.mkString(" "))

  def get: Set[List[Action]] = {
    solutions.toSet
  }

  def this(actions: List[Action]) {
    this(mutable.Set(actions))
  }
  def this(actions: Action*) {
    this(actions.toList)
  }
  def this() {
    this(mutable.Set.empty[List[Action]])
  }

  def removeAll(predicate: List[Action] => Boolean): Unit = {
    solutions.foreach(s => if (predicate(s)) {
      solutions.remove(s)
      solutionHash.remove(s.mkString(" "))
    })
  }

  def isEmpty: Boolean = {
    solutions.isEmpty
  }
  def cross(s: SolutionSet): SolutionSet = {
    if (s.isEmpty)
      new SolutionSet()
    else
      new SolutionSet(solutions.flatMap(x => s.solutions.map(y => x ::: y)))
  }
  def cross(s: List[Action]): SolutionSet = {
    new SolutionSet(solutions.map(x => s ::: x))
  }
  def cross(a: Action): SolutionSet = {
    new SolutionSet(solutions.map(x => a :: x))
  }

  def merge(s: SolutionSet): SolutionSet = {
    val x = new SolutionSet()
    x.add(this)
    x.add(s)
    x
  }

  def add(s: List[Action]): Unit = {
    if (!solutionHash.contains(s.mkString(" "))) {
      solutionHash += s.mkString(" ")
      solutions += s
    }
  }

  def add(s: SolutionSet): Unit = {
    s.solutions.foreach(this.add)
  }

  def x(a: Action): SolutionSet = cross(a)
  def x(s: List[Action]): SolutionSet = cross(s)
  def xx(s: SolutionSet): SolutionSet = cross(s)
  def ++=(s: SolutionSet): Unit = add(s)
  def +=(s: List[Action]): Unit = add(s)
  def ++(s: SolutionSet): SolutionSet = merge(s)

  override def toString: String = solutions.mkString(", ")
}

object SolutionSet {
  def noSolution = new SolutionSet(mutable.Set.empty[List[Action]])
  def successMark = new SolutionSet(List.empty[Action])
}