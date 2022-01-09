package lofi

import Types.{Real, Time}

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.language.{implicitConversions, postfixOps}
import scala.math.*

sealed trait Observable[A] {
  def unary_- : Observable[Real] = Lift1R(this.asInstanceOf[Observable[Real]], Negate)

  def +(that: Observable[Real]): Observable[Real] = Lift2R(this.asInstanceOf[Observable[Real]], that, Add)

  def -(that: Observable[Real]): Observable[Real] = Lift2R(this.asInstanceOf[Observable[Real]], that, Sub)

  def *(that: Observable[Real]): Observable[Real] = Lift2R(this.asInstanceOf[Observable[Real]], that, Mul)

  def /(that: Observable[Real]): Observable[Real] = Lift2R(this.asInstanceOf[Observable[Real]], that, Div)

  def **(that: Observable[Real]): Observable[Real] = Lift2R(this.asInstanceOf[Observable[Real]], that, Pow)

  def ^(that: Observable[Real]): Observable[Real] = Lift2R(this.asInstanceOf[Observable[Real]], that, Max)

  def <=(that: Observable[Real]): Observable[Boolean] = Comp2R(this.asInstanceOf[Observable[Real]], that, Leq)

  def >=(that: Observable[Real]): Observable[Boolean] = Comp2R(this.asInstanceOf[Observable[Real]], that, Geq)

  def <(that: Observable[Real]): Observable[Boolean] = Comp2R(this.asInstanceOf[Observable[Real]], that, Lt)

  def >(that: Observable[Real]): Observable[Boolean] = Comp2R(this.asInstanceOf[Observable[Real]], that, Gt)

  def ==(that: Observable[Real]): Observable[Boolean] = Comp2R(this.asInstanceOf[Observable[Real]], that, Eq)

  def !=(that: Observable[Real]): Observable[Boolean] = Comp2R(this.asInstanceOf[Observable[Real]], that, Neq)

  def unary_! : Observable[Boolean] = Lift1B(this.asInstanceOf[Observable[Boolean]], Not)

  def &(that: Observable[Boolean]): Observable[Boolean] = Lift2B(this.asInstanceOf[Observable[Boolean]], that, And)

  def |(that: Observable[Boolean]): Observable[Boolean] = Lift2B(this.asInstanceOf[Observable[Boolean]], that, Or)
}

case object Now extends Observable[Time]

case class Const[A](a: A) extends Observable[A]

case class Lookup(id: String) extends Observable[Real]

case class Lift1R(o: Observable[Real], op: UnROp) extends Observable[Real]

case class Lift2R(l: Observable[Real], r: Observable[Real], op: BinROp) extends Observable[Real]

case class ReduceR(o: Observable[Real], origin: Time, z: Real, op: BinROp) extends Observable[Real]

case class Comp2R(l: Observable[Real], r: Observable[Real], op: CompROp) extends Observable[Boolean]

case class Lift1B(o: Observable[Boolean], op: UnBOp) extends Observable[Boolean]

case class Lift2B(l: Observable[Boolean], r: Observable[Boolean], op: BinBOp) extends Observable[Boolean]

case class Cond[A](b: Observable[Boolean], l: Observable[A], r: Observable[A]) extends Observable[A]

case class Freeze[A](o: Observable[A], ts: List[Time]) extends Observable[A]

case class Worst(os: List[Observable[Real]], n: Int) extends Observable[Real]

case class Expectation(o: Observable[Real], t: Time) extends Observable[Real]

case class Model(a: Observable[Real], b: Observable[Real], origin: Time, init: Real, id: String, f: UnROp) extends Observable[Real]

sealed trait DiscountingType
case object Continuous extends DiscountingType

case class DiscountFactor(r: Real, maturity: Time, dType: DiscountingType) extends Observable[Real]


object Observable {

  val now: Observable[Time] = Now

  implicit def realToObservable(a: Real): Observable[Real] = Const(a)

  implicit def booleanToObservable(b: Boolean): Observable[Boolean] = Const(b)

  def exp(o: Observable[Real]): Observable[Real] = Lift1R(o, Exp)

  def lookup(id: String): Observable[Real] = Lookup(id)

  def ever(b: Observable[Boolean]): Observable[Boolean] = Lift1B(b, Ever)

  def always(b: Observable[Boolean]): Observable[Boolean] = Lift1B(b, Always)

  def cond[A](b: Observable[Boolean])(l: Observable[A])(r: Observable[A]): Observable[A] = Cond(b, l, r)

  def freeze[A](o: Observable[A])(ts: List[Time]): Observable[A] = Freeze(o, ts)

  def worst(os: List[Observable[Real]])(n: Int): Observable[Real] = Worst(os, n)

  def average(o: Observable[Real])(origin: Time): Observable[Real] = ReduceR(o, origin, 0.0, Avg)

  def max(o: Observable[Real])(origin: Time): Observable[Real] = ReduceR(o, origin, Double.MinValue, Max)

  def performance(o: Observable[Real])(origin: Time): Observable[Real] = (o / freeze(o)(origin :: Nil)) - 1.0

  def expectation(o: Observable[Real])(t: Time): Observable[Real] = Expectation(o, t)
  
  def cdiscount(r: Real)(maturity: Time): Observable[Real] = DiscountFactor(r, maturity, Continuous)

  def pp[A](input: Observable[A]): String = input match {
    case Now => "t"
    case Const(a) => s"$a"
    case Lookup(id) => s"S_{$id}(t)"
    case Lift1R(o, Negate) => s"-${pp(o)}"
    case Lift1R(o, Exp) => s"exp(${pp(o)})"
    case Lift1R(o, Log) => s"log(${pp(o)})"
    case Lift1R(o, Sin) => s"sin(${pp(o)})"
    case Lift1R(o, Cos) => s"cos(${pp(o)})"
    case Lift2R(l, r, Add) => s"${pp(l)} + ${pp(r)}"
    case Lift2R(l, r, Sub) => s"${pp(l)} - ${pp(r)}"
    case Lift2R(l, r, Mul) => s"${pp(l)} * ${pp(r)}"
    case Lift2R(l, r, Div) => s"${pp(l)} / ${pp(r)}"
    case Lift2R(l, r, Max) => s"max(${pp(l)}, ${pp(r)})"
    case Lift2R(l, r, Pow) => s"pow(${pp(l)}, ${pp(r)})"
    case ReduceR(o, origin, z, Add) => s"sum(${pp(o)})"
    case ReduceR(o, origin, z, Mul) => s"prd(${pp(o)})"
    case ReduceR(o, origin, z, Max) => s"max(${pp(o)})"
    case ReduceR(o, origin, z, Avg) => s"avg(${pp(o)})"
    case Comp2R(l, r, Leq) => s"${pp(l)} <= ${pp(r)}"
    case Comp2R(l, r, Geq) => s"${pp(l)} >= ${pp(r)}"
    case Comp2R(l, r, Lt)  => s"${pp(l)} <  ${pp(r)}"
    case Comp2R(l, r, Gt)  => s"${pp(l)} >  ${pp(r)}"
    case Comp2R(l, r, Eq)  => s"${pp(l)} == ${pp(r)}"
    case Comp2R(l, r, Neq) => s"${pp(l)} != ${pp(r)}"
    case Lift1B(o, Not) => s"!${pp(o)}"
    case Lift1B(o, Ever) => s"ever(${pp(o)})"
    case Lift1B(o, Always) => s"always(${pp(o)})"
    case Lift2B(l, r, And) => s"${pp(l)} and ${pp(r)}"
    case Lift2B(l, r, Or) => s"${pp(l)} or ${pp(r)}"
    case Cond(b, l, r) => s"if(${pp(b)})(${pp(l)})(${pp(r)})"
    case Freeze(o, s) => s"${pp(o)}_{t=$s}"
    case Worst(os, n) => s"worst(${os.map(o => pp(o)).foldLeft("")((l, r) => l + ", " + r)}; n=$n)"
    case Expectation(o, s) => s"E(${pp(o)}(s=$s) | t)"
    case Model(a, b, origin, init, id, f) => s"model(a=${pp(a)}, b=${pp(b)}, origin=$origin, init=$init, id=$id)"
    case _ => s"{Unknown}"
  }

  def evaluate[A](evaluationContext: EvaluationContext)(simulationContext: SimulationContext)(input: Observable[A])(t: Time): A = input match {
    case Now => t
    case Const(a) => a
    case Lookup(id) => evaluationContext.lookup(id)(t)
    case Lift1R(o, Negate) => -evaluate(evaluationContext)(simulationContext)(o)(t)
    case Lift1R(o, Exp) => scala.math.exp(evaluate(evaluationContext)(simulationContext)(o)(t))
    case Lift1R(o, Log) => scala.math.log(evaluate(evaluationContext)(simulationContext)(o)(t))
    case Lift1R(o, Sin) => scala.math.sin(evaluate(evaluationContext)(simulationContext)(o)(t))
    case Lift1R(o, Cos) => scala.math.cos(evaluate(evaluationContext)(simulationContext)(o)(t))
    case Lift2R(l, r, Add) => evaluate(evaluationContext)(simulationContext)(l)(t) + evaluate(evaluationContext)(simulationContext)(r)(t)
    case Lift2R(l, r, Sub) => evaluate(evaluationContext)(simulationContext)(l)(t) - evaluate(evaluationContext)(simulationContext)(r)(t)
    case Lift2R(l, r, Mul) => evaluate(evaluationContext)(simulationContext)(l)(t) * evaluate(evaluationContext)(simulationContext)(r)(t)
    case Lift2R(l, r, Div) => evaluate(evaluationContext)(simulationContext)(l)(t) / evaluate(evaluationContext)(simulationContext)(r)(t)
    case Lift2R(l, r, Max) => scala.math.max(evaluate(evaluationContext)(simulationContext)(l)(t), evaluate(evaluationContext)(simulationContext)(r)(t))
    case Lift2R(l, r, Pow) => pow(evaluate(evaluationContext)(simulationContext)(l)(t), evaluate(evaluationContext)(simulationContext)(r)(t))
    case ReduceR(o, origin, z, Add) => evaluateReduceR(evaluationContext)(simulationContext)(o)(origin)(z)(_+_)(t)
    case ReduceR(o, origin, z, Mul) => evaluateReduceR(evaluationContext)(simulationContext)(o)(origin)(z)(_*_)(t)
    case ReduceR(o, origin, z, Max) => evaluateReduceR(evaluationContext)(simulationContext)(o)(origin)(z)(scala.math.max)(t)
    case ReduceR(o, origin, z, Avg) => evaluateReduceR(evaluationContext)(simulationContext)(o)(origin)(z)(_+_)(t) / evaluationContext.times.count(s => origin <= s && s <= t).toDouble
    case Comp2R(l, r, Leq) => evaluate(evaluationContext)(simulationContext)(l)(t) <= evaluate(evaluationContext)(simulationContext)(r)(t)
    case Comp2R(l, r, Geq) => evaluate(evaluationContext)(simulationContext)(l)(t) >= evaluate(evaluationContext)(simulationContext)(r)(t)
    case Comp2R(l, r, Lt)  => evaluate(evaluationContext)(simulationContext)(l)(t) <  evaluate(evaluationContext)(simulationContext)(r)(t)
    case Comp2R(l, r, Gt)  => evaluate(evaluationContext)(simulationContext)(l)(t) >  evaluate(evaluationContext)(simulationContext)(r)(t)
    case Comp2R(l, r, Eq)  => evaluate(evaluationContext)(simulationContext)(l)(t) == evaluate(evaluationContext)(simulationContext)(r)(t)
    case Comp2R(l, r, Neq) => evaluate(evaluationContext)(simulationContext)(l)(t) != evaluate(evaluationContext)(simulationContext)(r)(t)
    case Lift1B(o, Not) => !evaluate(evaluationContext)(simulationContext)(o)(t)
    case Lift1B(o, Ever) => evaluationContext.times.filter(_ <= t).exists(s => evaluate(evaluationContext)(simulationContext)(o)(s))
    case Lift1B(o, Always) => evaluationContext.times.filter(_ <= t).forall(s => evaluate(evaluationContext)(simulationContext)(o)(s))
    case Lift2B(l, r, And) => evaluate(evaluationContext)(simulationContext)(l)(t) & evaluate(evaluationContext)(simulationContext)(r)(t)
    case Lift2B(l, r, Or) => evaluate(evaluationContext)(simulationContext)(l)(t) | evaluate(evaluationContext)(simulationContext)(r)(t)
    case Cond(b, l, r) => if (evaluate(evaluationContext)(simulationContext)(b)(t)) evaluate(evaluationContext)(simulationContext)(l)(t) else evaluate(evaluationContext)(simulationContext)(r)(t)
    case Freeze(o, Nil) => evaluate(evaluationContext)(simulationContext)(o)(t)
    case Freeze(o, s :: Nil) => evaluate(evaluationContext)(simulationContext)(o)(s)
    case Freeze(o, ts) if t < ts.min => evaluate(evaluationContext)(simulationContext)(o)(ts.min)
    case Freeze(o, ts) =>
      val s = ts.filter(_ <= t).max
      evaluate(evaluationContext)(simulationContext)(o)(s)
    case Worst(os, n) => os.map(o => evaluate(evaluationContext)(simulationContext)(o)(t)).sorted.apply(n)
    case Expectation(o, s) => evaluateExpectation(evaluationContext)(simulationContext)(o)(s)(t)
    case Model(a, b, origin, init, id, f) => ???
    case _ => ???
  }

  def evaluateAll[A](evaluationContext: EvaluationContext)
                    (simulationContext: SimulationContext)
                    (o: Observable[A])
  : SortedMap[Time, A]
  =
    val result = evaluationContext
      .times
      .map(t => (t, evaluate(evaluationContext)(simulationContext)(o)(t)))
    SortedMap(result: _*)

  private def evaluateReduceR(evaluationContext: EvaluationContext)
                             (simulationContext: SimulationContext)
                             (o: Observable[Real])
                             (origin: Time)
                             (z: Real)
                             (f: (Real, Real) => Real)
                             (t: Time)
  : Real =
    val tmp1 = evaluationContext
    .times
    .filter(origin <= _).filter(_ <= t)
    .map(s => evaluate(evaluationContext)(simulationContext)(o)(s))
    val tmp2 = tmp1
    .foldLeft(z)((x, y) => f(x, y))
    //println(s"Evaluating reducer: evals=$tmp1, result=$tmp2")
    tmp2

  private def evaluateExpectation(evaluationContext: EvaluationContext)
                                 (simulationContext: SimulationContext)
                                 (o: Observable[Real])
                                 (s: Time)
                                 (t: Time)
  : Real =
    //println(s"*** Evaluating expectation, simulatin ${pp(o)}")
    if (t < s)
      val ts = t :: simulationContext.times
      val sim_o = simulate(evaluationContext.filter(_ <= t))(simulationContext)(o)
      //println(s"Simulation done with origin=$t: $sim_o")
      // above we should be simulating using value of o at t
      // for that we can for example filter evaluationContext to leave only times <= t
      val tmp1 = ts.zip(sim_o.transpose)
      //println(s"tmp1: $tmp1")
      val tmp2 = tmp1.toMap
      //println(s"tmp2: $tmp2")
      val o_Ts = tmp2(s)
      o_Ts.sum / o_Ts.length
    else
      evaluate(evaluationContext)(simulationContext)(o)(t)

  //@tailrec
  def simulate[A](evaluationContext: EvaluationContext)(simulationContext: SimulationContext)(input: Observable[A]): List[List[A]] = input match {
    case Const(a) => (1 to simulationContext.pathCount).toList.map(_ => simulationContext.times.map(_ => a))
    case Lookup(id) =>
      val origin = evaluationContext.times.max
      val init = evaluationContext.lookup(id)(origin)
      simulate(evaluationContext)(simulationContext)(simulationContext.models(id)(origin, init))
    case Lift1R(o, Negate) => simulateLift1(evaluationContext)(simulationContext)(o)(-_)
    case Lift1R(o, Exp) => simulateLift1(evaluationContext)(simulationContext)(o)(scala.math.exp)
    case Lift1R(o, Log) => simulateLift1(evaluationContext)(simulationContext)(o)(scala.math.log)
    case Lift1R(o, Sin) => simulateLift1(evaluationContext)(simulationContext)(o)(scala.math.sin)
    case Lift1R(o, Cos) => simulateLift1(evaluationContext)(simulationContext)(o)(scala.math.cos)
    case Lift2R(l, r, Add) => simulateLift2(evaluationContext)(simulationContext)(l)(r)(_ + _)
    case Lift2R(l, r, Sub) => simulateLift2(evaluationContext)(simulationContext)(l)(r)(_ - _)
    case Lift2R(l, r, Mul) => simulateLift2(evaluationContext)(simulationContext)(l)(r)(_ * _)
    case Lift2R(l, r, Div) => simulateLift2(evaluationContext)(simulationContext)(l)(r)(_ / _)
    case Lift2R(l, r, Max) => simulateLift2(evaluationContext)(simulationContext)(l)(r)(scala.math.max)
    case Lift2R(l, r, Pow) => simulateLift2(evaluationContext)(simulationContext)(l)(r)(scala.math.pow)
    case ReduceR(o, origin, z, Add) => simulateReduce(evaluationContext)(simulationContext)(o)(origin)(z)(_ + _)
    case ReduceR(o, origin, z, Mul) => simulateReduce(evaluationContext)(simulationContext)(o)(origin)(z)(_ * _)
    case ReduceR(o, origin, z, Max) => simulateReduce(evaluationContext)(simulationContext)(o)(origin)(z)(scala.math.max)
    case ReduceR(o, origin, z, Avg) =>
      val times = simulationContext.times
      val sim_o_1 = simulate(evaluationContext)(simulationContext)(o)
      val sim_o_2 = sim_o_1.map(path => times.zip(path).toMap)
      val sim_o = (1 to simulationContext.pathCount).toList.zip(sim_o_2).toMap
      (1 to simulationContext.pathCount).toList.map(i => times.map(t => times.filter(origin <= _).filter(_ <= t).map(s => sim_o(i)(s)).foldLeft(z)(_ + _) / times.count(s => origin <= s && s <= t).toDouble))
    case Comp2R(l, r, Leq) => simulateLift2(evaluationContext)(simulationContext)(l)(r)(_ <= _)
    case Comp2R(l, r, Geq) => simulateLift2(evaluationContext)(simulationContext)(l)(r)(_ >= _)
    case Comp2R(l, r, Lt)  => simulateLift2(evaluationContext)(simulationContext)(l)(r)(_ <  _)
    case Comp2R(l, r, Gt)  => simulateLift2(evaluationContext)(simulationContext)(l)(r)(_ >  _)
    case Comp2R(l, r, Eq)  => simulateLift2(evaluationContext)(simulationContext)(l)(r)(_ == _)
    case Comp2R(l, r, Neq) => simulateLift2(evaluationContext)(simulationContext)(l)(r)(_ != _)
    case Lift1B(o, Not) => simulateLift1(evaluationContext)(simulationContext)(o)(!_)
    case Lift1B(o, Ever) =>
      val times = simulationContext.times
      val sim_o_1 = simulate(evaluationContext)(simulationContext)(o)
      val sim_o_2 = sim_o_1.map(path => times.zip(path).toMap)
      val sim_o = (1 to simulationContext.pathCount).toList.zip(sim_o_2).toMap
      (1 to simulationContext.pathCount).toList.map(i => times.map(t => times.filter(_ <= t).exists(s => sim_o(i)(s))))
    case Lift1B(o, Always) =>
      val times = simulationContext.times
      val sim_o_1 = simulate(evaluationContext)(simulationContext)(o)
      val sim_o_2 = sim_o_1.map(path => times.zip(path).toMap)
      val sim_o = (1 to simulationContext.pathCount).toList.zip(sim_o_2).toMap
      (1 to simulationContext.pathCount).toList.map(i => times.map(t => times.filter(_ <= t).forall(s => sim_o(i)(s))))
    case Lift2B(l, r, And) => simulateLift2(evaluationContext)(simulationContext)(l)(r)(_ & _)
    case Lift2B(l, r, Or) => simulateLift2(evaluationContext)(simulationContext)(l)(r)(_ | _)
    case Cond(b, l, r) =>
      val sim_b = simulate(evaluationContext)(simulationContext)(b)
      val sim_l = simulate(evaluationContext)(simulationContext)(l)
      val sim_r = simulate(evaluationContext)(simulationContext)(r)
      (sim_b zip (sim_l zip sim_r)) map {
        case (b_paths, (l_paths, r_paths)) => (b_paths zip (l_paths zip r_paths)) map {
          case (b_i, (l_i, r_i)) => if (b_i) l_i else r_i
        }
      }
    case Freeze(o, Nil) => simulate(evaluationContext)(simulationContext)(o)
    case Freeze(o, s :: Nil) =>
      val o_s = evaluate(evaluationContext)(simulationContext)(o)(s)
      (1 to simulationContext.pathCount).toList.map(i => simulationContext.times.map(t => o_s))
    case Freeze(o, ts) =>
      ???
    case Worst(os, n) => ???
    case Expectation(o, s) => ???
    case Model(a, b, origin, init, id, Negate) => simulateModel(evaluationContext)(simulationContext)(a)(b)(origin)(init)(id)(-_)(-_)
    case Model(a, b, origin, init, id, Exp) => simulateModel(evaluationContext)(simulationContext)(a)(b)(origin)(init)(id)(scala.math.exp)(scala.math.log)
    case Model(a, b, origin, init, id, Log) => simulateModel(evaluationContext)(simulationContext)(a)(b)(origin)(init)(id)(scala.math.log)(scala.math.exp)
    case _ => ???
  }

  private def simulateLift1[A](evaluationContext: EvaluationContext)
                            (simulationContext: SimulationContext)
                            (o: Observable[A])
                            (f: A => A)
  : List[List[A]] =
    simulate(evaluationContext)(simulationContext)(o) map (path => path map f)

  private def simulateLift2[A, B](evaluationContext: EvaluationContext)
                            (simulationContext: SimulationContext)
                            (l: Observable[A])
                            (r: Observable[A])
                            (f: (A, A) => B)
  : List[List[B]] = {
    val sim_l = simulate(evaluationContext)(simulationContext)(l)
    val sim_r = simulate(evaluationContext)(simulationContext)(r)
    (sim_l zip sim_r) map { case (ls, rs) => (ls zip rs) map ((x, y) => f(x, y)) }
  }

  private def simulateReduce(evaluationContext: EvaluationContext)
                            (simulationContext: SimulationContext)
                            (o: Observable[Real])
                            (origin: Time)
                            (z: Real)
                            (f: (Real, Real) => Real)
  : List[List[Real]] = {
    val times = simulationContext.times
    val sim_o_1 = simulate(evaluationContext)(simulationContext)(o)
    val sim_o_2 = sim_o_1.map(path => times.zip(path).toMap)
    val sim_o = (1 to simulationContext.pathCount).toList.zip(sim_o_2).toMap
    (1 to simulationContext.pathCount).toList.map(i => times.map(t => times.filter(origin <= _).filter(_ <= t).map(s => sim_o(i)(s)).foldLeft(z)(f)))
  }

  private def simulateModel(evaluationContext: EvaluationContext)
                           (simulationContext: SimulationContext)
                           (a: Observable[Real])
                           (b: Observable[Real])
                           (origin: Time)
                           (init: Real)
                           (id: String)
                           (f: Real => Real)
                           (g: Real => Real)
  : List[List[Real]] = {
    //println(s"Simulating model with origin=$origin and init=$init")
    val ts = simulationContext.times
    val a_origin = evaluate(evaluationContext)(simulationContext)(a)(origin)
    val sim_a = simulate(evaluationContext)(simulationContext)(a).map(p => a_origin :: p.dropRight(1))
    val b_origin = evaluate(evaluationContext)(simulationContext)(b)(origin)
    val sim_b = simulate(evaluationContext)(simulationContext)(b).map(p => b_origin :: p.dropRight(1))

    val result = (simulationContext.data(id)(origin) zip (sim_a zip sim_b)) map {
      case (wts, (as, bs)) => ((origin, ts.head) :: ts.zip(ts.tail))
        .zip((0.0, wts.head) :: wts.zip(wts.tail))
        .zip(as)
        .zip(bs)
        .map {
          case ((((t_prev, t), (wt_prev, wt)), a_t_prev), b_t_prev) => a_t_prev*(t-t_prev) + b_t_prev*(wt-wt_prev)
        }
        //.scanLeft(init)(_+_)
        .scanLeft(f(init))(_+_)
        .map(g)
    }
    result
  }

  def shouldEnclose[A](input: Observable[A]): Boolean = input match {
    case Now => false
    case Const(_) => false
    case Lookup(_) => false
    case Lift1R(_, _) => false
    case Lift2R(_, _, Add) => true
    case Lift2R(_, _, Sub) => true
    case Lift2R(_, _, Mul) => false
    case Lift2R(_, _, Div) => true
    case ReduceR(_, _, _, Add) => true
    case ReduceR(o, origin, z, Mul) => true
    case ReduceR(o, origin, z, Max) => true
    case ReduceR(o, origin, z, Avg) => true
    // ...
    case Expectation(_, _) => false
    case _ => true
  }

  def toTex[A](input: Observable[A])(time: String): String = input match {
    case Now => time
    case Const(a) => s"$a"
    case Lookup(id) => s"S_{$time}^{$id}"
    case Lift1R(o @ Now, Negate) => s"-${toTex(o)(time)}"
    case Lift1R(o @ Const(a), Negate) => s"-${toTex(o)(time)}"
    case Lift1R(o @ Lookup(id), Negate) => s"-${toTex(o)(time)}"
    case Lift1R(o, Negate) => s"-(${toTex(o)(time)})"
    case Lift1R(o, Exp) => s"e^{${toTex(o)(time)}}"
    case Lift1R(o, Log) => s"\\log{${toTex(o)(time)}}"
    case Lift1R(o, Sin) => s"\\sin{${toTex(o)(time)}}"
    case Lift1R(o, Cos) => s"\\cos{${toTex(o)(time)}}"
    case Lift2R(l, r, Add) => s"${toTex(l)(time)} + ${toTex(r)(time)}"
    case Lift2R(l, r, Sub) => s"${toTex(l)(time)} - ${toTex(r)(time)}"
    case Lift2R(l, r, Mul) =>
      val lTex = toTex(l)(time)
      val rTex = toTex(r)(time)
      val shouldEnclose_l = shouldEnclose(l)
      val shouldEnclose_r = shouldEnclose(r)
      val lRepr = if shouldEnclose_l then s"($lTex)" else lTex
      val rRepr = if shouldEnclose_r then s"($rTex)" else rTex
      if !shouldEnclose_l && !shouldEnclose_r then s"$lRepr*$rRepr" else s"$lRepr$rRepr"
    case Lift2R(l, r, Div) =>
      val lTex = toTex(l)(time)
      val rTex = toTex(r)(time)
      val lRepr = if shouldEnclose(l) then s"($lTex)" else lTex
      val rRepr = if shouldEnclose(r) then s"($rTex)" else rTex
      s"$lRepr/$rRepr"
    case Lift2R(Const(0), r, Max) =>
      val rTex = toTex(r)(time)
      val rRepr = if shouldEnclose(r) then s"[$rTex]" else rTex
      s"${rRepr}_+"
    case Lift2R(l, Const(0), Max) =>
      val lTex = toTex(l)(time)
      val lRepr = if shouldEnclose(l) then s"[$lTex]" else lTex
      s"${lRepr}_+"
    case Lift2R(l, r, Max) => s"\\max{${toTex(l)(time)}, ${toTex(r)(time)}}"
    case Lift2R(l, r, Pow) =>
      val lTex = toTex(l)(time)
      val lRepr = if shouldEnclose(l) then s"($lTex)" else lTex
      s"$lRepr^{${toTex(r)(time)}}"
    case ReduceR(o, origin, z, Add) => val s = "s"; s"\\sum_{t_0 := $origin}^{$time}{${toTex(o)(s)}}"
    case ReduceR(o, origin, z, Mul) => val s = "s"; s"\\prod_{t_0 := $origin}^{$time}{${toTex(o)(s)}}"
    case ReduceR(o, origin, z, Max) => val s = "s"; s"\\max_{t_0 := $origin \\leq s \\leq $time}{${toTex(o)(s)}}"
    case ReduceR(o, 0.0, z, Avg) => val s = "s"; s"\\frac{1}{$time}\\int_{0}^{$time}{${toTex(o)(s)} ds}"
    case ReduceR(o, origin, z, Avg) => val s = "s"; s"\\frac{1}{($time - t_0)}\\int_{t_0}^{$time}{${toTex(o)(s)} ds}"
    case Comp2R(l, r, Leq) => s"${toTex(l)(time)} <= ${toTex(r)(time)}"
    case Comp2R(l, r, Geq) => s"${toTex(l)(time)} >= ${toTex(r)(time)}"
    case Comp2R(l, r, Lt)  => s"${toTex(l)(time)} <  ${toTex(r)(time)}"
    case Comp2R(l, r, Gt)  => s"${toTex(l)(time)} >  ${toTex(r)(time)}"
    case Comp2R(l, r, Eq)  => s"${toTex(l)(time)} == ${toTex(r)(time)}"
    case Comp2R(l, r, Neq) => s"${toTex(l)(time)} != ${toTex(r)(time)}"
    case Lift1B(o, Not) =>
      val oTex = toTex(o)(time)
      val oRepr = if shouldEnclose(o) then s"($oTex)" else oTex
      s"\\neg{$oTex}"
    case Lift1B(o, Ever) => val underlyingTime = "s"; s"\\exists s \\in \\mathbb{T}; ${toTex(o)(underlyingTime)}"
    case Lift1B(o, Always) => val underlyingTime = "s"; s"\\forall s \\in \\mathbb{T}; ${toTex(o)(underlyingTime)}"
    case Lift2B(l, r, And) =>
      val lTex = toTex(l)(time)
      val rTex = toTex(r)(time)
      val lRepr = if shouldEnclose(l) then s"($lTex)" else lTex
      val rRepr = if shouldEnclose(r) then s"($rTex)" else rTex
      s"$lRepr \\land $rRepr"
    case Lift2B(l, r, Or) =>
      val lTex = toTex(l)(time)
      val rTex = toTex(r)(time)
      val lRepr = if shouldEnclose(l) then s"($lTex)" else lTex
      val rRepr = if shouldEnclose(r) then s"($rTex)" else rTex
      s"$lRepr \\lor $rRepr"
    case Cond(b, l, r) =>
      val lTex = toTex(l)(time)
      val lRepr = if shouldEnclose(l) then s"($lTex)" else lTex
      s"$lRepr \\chi_{${toTex(b)(time)}}"
    case Freeze(o, s) =>
      ???
    case Worst(os, n) =>
      ???
    case Expectation(o, s) =>
      s"\\mathbb{E}(${toTex(o)("T")} | \\mathcal{F}_{$time})"
    case Model(a, b, origin, init, id, Log) =>
      s"\\tilde{S_{$time}^{$id}}"
    case DiscountFactor(r, maturity, Continuous) =>
      s"e^{-r*(T-$time)}"
    case _ => s"{Unknown}"
  }
}
