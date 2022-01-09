package lofi

import Types.*

import scala.language.postfixOps
import scala.collection.immutable.SortedMap

case class EvaluationContext(data: Map[String, SortedMap[Time, Real]]) {
  val times: List[Time] = data.toList.flatMap((_, m) => m.toList.map((t, _) => t)).distinct
  def lookup(id: String)(t: Time): Real = data(id)(t)
  def shift(id: String)(dx: Real): EvaluationContext = {
    val shiftedData = data map {
      case (str, values) if str == id => (str, values map { case (s, x) => (s, x+dx) })
      case (str, values) => (str, values)
    }
    EvaluationContext(shiftedData)
  }
  def filter(f: Time => Boolean): EvaluationContext =
    val filteredData = data.map{ case (id, mdata) => (id, mdata.filter((t, _) => f(t))) }
    EvaluationContext(filteredData)
}
