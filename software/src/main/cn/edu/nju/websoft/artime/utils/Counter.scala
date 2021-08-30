package cn.edu.nju.websoft.artime.utils

import scala.collection.mutable

case class Counter[T](
  map: mutable.Map[T, Int]
) {
  assert(!map.values.exists(_.<(0)))

  def isEmpty: Boolean = map.values.forall(_ == 0)

  def get(v: T): Int = map.getOrElse(v, 0)
  def contains(v: T): Boolean = map.contains(v)
  def use(v: T): Boolean = {
    if (contains(v)) {
      if (get(v) <= 1) map.remove(v)
      else map.update(v, get(v) - 1)
      true
    } else
      false
  }
  def add(v: T): Unit = {
    if (contains(v)) map.update(v, get(v) + 1)
    else map.update(v, 1)
  }

  def addAll(vs: Seq[T]): Unit = {
    vs.foreach(add)
  }

  def this(m: Map[T, Int]) = {
    this(mutable.Map(m.toSeq: _*))
  }
  def this(s: Seq[T]) = {
    this(s.groupBy(x => x).mapValues(_.length))
  }
}
