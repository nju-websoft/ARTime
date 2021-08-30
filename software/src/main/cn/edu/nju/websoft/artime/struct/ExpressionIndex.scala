package cn.edu.nju.websoft.artime.struct

/**
 * @param docId
 * @param begin
 * @param end
 */
case class ExpressionIndex(
 docId: Int,
 begin: Int,
 end: Int
) extends Comparable[ExpressionIndex] {
  def isContainedIn(x: ExpressionIndex): Boolean = {
    docId == x.docId && begin >= x.begin && end <= x.end
  }
  def length: Int = end - begin

  def containedCoverage(x: ExpressionIndex): Double = {
    if (isContainedIn(x)) length.toDouble / x.length else 0.0
  }

  def overlap(x: ExpressionIndex): Boolean = {
    docId == x.docId && Math.max(begin, x.begin) < Math.min(end, x.end)
  }

  def overlapOrConsecutive(x: ExpressionIndex): Boolean = {
    docId == x.docId && Math.max(begin, x.begin) <= Math.min(end, x.end)
  }

  def before(x: ExpressionIndex): Boolean = {
    docId < x.docId || (docId == x.docId && end <= x.begin)
  }
  def beforeOrOverlap(x: ExpressionIndex): Boolean = {
    docId < x.docId || (docId == x.docId && begin < x.end)
  }

  override def compareTo(o: ExpressionIndex): Int = {
    if (docId != o.docId) docId - o.docId
    else if (begin != o.begin) begin - o.begin
    else end - o.end
  }
}
