package lofi

import Types.{Real, Time}

import scala.collection.immutable.SortedMap

case class SimulationContext(data: Map[String, Map[Time, List[List[Real]]]], models: Map[String, (Time, Real) => Observable[Real]]
                            , times: List[Time], pathCount: Int) {
  def getPathsAsContexts: List[EvaluationContext] = Nil // Why is this needed ???
}
