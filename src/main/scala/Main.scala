import lofi._
import lofi.Observable._
import lofi.Types._
import org.apache.commons.math3.distribution.NormalDistribution

import scala.collection.immutable.SortedMap
import scala.language.postfixOps

object Main {
    private def testObs[A](evaluationContext: EvaluationContext)(simulationContext: SimulationContext)(o: Observable[A]) = {
      val eval_o = evaluateAll(evaluationContext)(simulationContext)(o)
      println("--------------------")
      println(s"o=${toTex(o)("t")}")
      println(s"eval(o)=$eval_o")
      println("--------------------")
    }

    private def generateBrownianIncrements(origin: Time)(times: List[Time])(r: scala.util.Random): List[Real]
    =
      (r.nextGaussian() * scala.math.sqrt(times.head-origin)) :: times
        .zip(times.tail)
        .map{ case (t0, t1) => t1 - t0 }
        .map(dt => r.nextGaussian() * scala.math.sqrt(dt))

    private def generateBrownianPath(origin: Time)(times: List[Time])(r: scala.util.Random): List[Real]
    =
      generateBrownianIncrements(origin)(times)(r).scanLeft(0.0)(_ + _).tail

    def bs(r: Real)(vol: Real)(spot: Real)(strike: Real)(t: Time)(T: Time) =
      val dt = T - t
      val d1 = (1/(vol*scala.math.sqrt(dt))) * (scala.math.log(spot/strike) + ((r + 0.5*vol*vol) * dt))
      val d2 = d1 - vol*scala.math.sqrt(dt)
      val normal = new NormalDistribution(0.0, 1.0)
      val Nd1 = normal.cumulativeProbability(d1)
      val Nd2 = normal.cumulativeProbability(d2)
      spot*Nd1 - strike*scala.math.exp(-r*(T-t))*Nd2

    def main(args: Array[String]): Unit = {
      val data = Map("AAPL" -> SortedMap(0.0 -> 100.0, 1.0 -> 98.76, 2.0 -> 93.52, 3.0 -> 101.78))/*.map((k, v) => (k, scala.math.log(v))))*/
      val symbols = Map("r" -> 0.05, "vol" -> 0.2, "T" -> 5.0, "K" -> 100.0, "B" -> 120.0)
      val evaluationContext = EvaluationContext(data, symbols)
      val ts = evaluationContext.times
      println(s"ts=$ts")
      println(s"data=$data")

      val pathCount = 1

      val simulation_times = List(3.05, 3.1, 3.15, 3.2, 3.25, 3.3, 3.35, 3.4, 3.45,
        3.5, 3.55, 3.6, 3.65, 3.7, 3.75, 3.8, 3.85, 3.9, 3.95, 4.0, 4.05, 4.1, 4.15,
        4.2, 4.25, 4.3, 4.35, 4.4, 4.45, 4.5, 4.55, 4.6, 4.65, 4.7, 4.75, 4.8, 4.85,
        4.9, 4.95, 5.0, 6.0)

      //val simulation_times = List(4.0, 5.0, 6.0)
      val rand = new scala.util.Random(0)
      val brownian_data = Map("AAPL" -> ts.map(t => (t, (1 to pathCount).toList.map(_ => generateBrownianPath(t)(simulation_times)(rand)))).toMap)
      //println(s"brownian_data=$brownian_data")
      val r = 0.05
      val vol = 0.2
      val models = Map("AAPL" -> ((origin: Time, init: Real) => Model(r-0.5*vol*vol, vol, origin, init, "AAPL", Log)))
      val simulationContext = SimulationContext(brownian_data, models, simulation_times, pathCount)

      //val o1 = (now + 1) ** now
      val o2 = lookup("AAPL")
      /*
      val o3 = 3 * o2 - 1
      val o4 = freeze(o3)(ts(2))
      val o5 = performance(o3)(ts(1))
      val o6 = worst(List(o3, o4))(0)
      val o7 = ever(o3 > 300)
      val o8 = always(o3 > 300)
      val o9 = always(o3 > 100)
      val o10 = average(o3)
      val o11 = max(o3)
      val o12 = cond(now <= ts(1))(now)(o11)
      */

      val symr = symbol("r")
      val symK = symbol("K")
      val symT = symbol("T")
      val symB = symbol("B")

      val T = 5.0
      val K = 100.0
      val S = o2
      //val S = exp(o2)
      //val o13 = expectation(o2)(T)
      val o13 = cdiscount(symr)(symT) * expectationT((S - symK) ^ 0)(symT)
      val o13_put = cdiscount(r)(T) * expectation((K - S) ^ 0)(T)
      val pcp = (o13 - o13_put) - (S - exp(-r * (T - now))*K) // should be near 0

      val L = 95.0
      val H = 105.0

      // ui
      val o14 = exp(-r * (T - now)) * expectation(cond(ever(S > H))((S - K) ^ 0)(0))(T)
      // di
      val o15 = cdiscount(r)(T) * expectation(cond(ever(S <= H))((S - K) ^ 0)(0))(T)
      // uo
      val o16 = cdiscount(r)(T) * expectation(cond(always(S <= H))((S - K) ^ 0)(0))(T)
      // do
      val E_S = expectation(S)(T)
      val o17 = cdiscount(r)(T) * expectation(cond(always(S > H))((S - K) ^ 0)(0))(T)
      //val o17 = exp(-r * (T - now)) * expectation(cond(always(S > H))((S - K) ^ 0)(0))(T)

      val bp1 = o14 + o16 - o13 // ui + uo = stc_call
      val bp2 = o15 + o17 - o13 // di + do = stc_call

      // lookback
      val lookback_origin = 0.0
      val o18 = cdiscount(r)(T) * expectation((max(S)(lookback_origin)-S) ^ 0)(T)

      // asian
      val asian_origin = 0.0
      //val o19 = exp(-r * (T - now)) * expectation((average(S)(asian_origin) - K) ^ 0)(T)
      val o19 = cdiscount(symr)(symT) * expectationT((average(S)(asian_origin) - symK) ^ 0)(symT)

      // next we need to change Cond(b, l, r) to Cond(b, o)

      // next we need to see how we can define a asian price with a given list of observation times,
      // instead of continuously observing (freeze observable in between any two elements of the input list of times)

      // next we need to look at how we can define a moving average (average over last n times)

      // next we need to define some useful functions such as:
      //  . isPathDependent
      //  . extractObservationTimes,
      //  . extractTimes
      //  . extractCashflows

      // next we need to find simplification rules, and define
      // following function:
      //  . simplify

      // next we need to define a function toTex

      // next we need to add multidimensional simulation capability

      // next we need to test worstOf

      // next we need to test optimiz best timing II without taking FX into account

      // next we need to add multiple currencies support

      // next we need to work more on models (try other models than bs and see how
      // it goes, what needs to be changed, ...)

      // next we need to add american option pricing capability

      //println("(1)")
      //testObs(evaluationContext)(simulationContext)(o1)
      println("(2)")
      testObs(evaluationContext)(simulationContext)(o2)
      /*
      println("(3)")
      testObs(evaluationContext)(simulationContext)(o3)
      println("(4)")
      testObs(evaluationContext)(simulationContext)(o4)
      println("(5)")
      testObs(evaluationContext)(simulationContext)(o5)
      println("(6)")
      testObs(evaluationContext)(simulationContext)(o6)
      println("(7)")
      testObs(evaluationContext)(simulationContext)(o7)
      println("(8)")
      testObs(evaluationContext)(simulationContext)(o8)
      println("(9)")
      testObs(evaluationContext)(simulationContext)(o9)
      println("(10)")
      testObs(evaluationContext)(simulationContext)(o10)
      println("(11)")
      testObs(evaluationContext)(simulationContext)(o11)
      println("(12)")
      testObs(evaluationContext)(simulationContext)(o12)
      */
      println("(S)")
      testObs(evaluationContext)(simulationContext)(S)
      //testObs(evaluationContext)(simulationContext)(E_S)
      println("(13)")
      testObs(evaluationContext)(simulationContext)(o13)
      //val prices = ts.map(t => (t, data("AAPL")(t) + (r - 0.5*vol*vol)*(T-t) )).toMap
      //val prices = ts.map(t => (t, scala.math.exp(data("AAPL")(t)) * scala.math.exp(r * (T-t)) )).toMap
      val prices = ts.map(t => (t, bs(r)(vol)(scala.math.exp(data("AAPL")(t)))(K)(t)(T))).toMap
      println(s"prices=$prices")
      println("(Put Call Parity)")
      testObs(evaluationContext)(simulationContext)(pcp)


      println("(Barrier Parity 1)")
      testObs(evaluationContext)(simulationContext)(bp1)
      println("(Barrier Parity 2)")
      testObs(evaluationContext)(simulationContext)(bp2)
      println("(14) (ui)")
      testObs(evaluationContext)(simulationContext)(o14)
      println("(16) (uo)")
      testObs(evaluationContext)(simulationContext)(o16)
      println("(15) (di)")
      testObs(evaluationContext)(simulationContext)(o15)
      println("(17) (do)")
      testObs(evaluationContext)(simulationContext)(o17)
      println("(18) (lookback)")
      testObs(evaluationContext)(simulationContext)(o18)
      println("(19) (asian)")
      testObs(evaluationContext)(simulationContext)(o19)


    }
}
