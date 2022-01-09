package lofi

sealed trait UnBOp

case object Not extends UnBOp

case object Ever extends UnBOp

case object Always extends UnBOp

sealed trait BinBOp

case object And extends BinBOp

case object Or extends BinBOp

sealed trait UnROp

case object Negate extends UnROp

case object Exp extends UnROp

case object Log extends UnROp

case object Sin extends UnROp

case object Cos extends UnROp

sealed trait BinROp

case object Add extends BinROp

case object Sub extends BinROp

case object Mul extends BinROp

case object Div extends BinROp

case object Max extends BinROp

case object Pow extends BinROp

case object Avg extends BinROp

sealed trait CompROp

case object Leq extends CompROp

case object Geq extends CompROp

case object Lt extends CompROp

case object Gt extends CompROp

case object Eq extends CompROp

case object Neq extends CompROp
