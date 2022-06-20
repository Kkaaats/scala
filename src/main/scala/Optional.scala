package BoolExpr
//import scala.annotation.varargs
//import com.tkroman.kpi.y2022.l1._

enum Op:
  case And, Or, Neg
  def toStr(op: Op = this): String =
    op match
      case And => "&&"
      case Or  => "||"
      case Neg => "!"

enum BoolExpr:
  case Nil
  case Lit(b: Boolean)
  case Variable(b: String)
  case Expr(op: Op, l: BoolExpr, r: BoolExpr)
  override def toString(): String =
    def run(e: BoolExpr = this): String =
      e match
        case Nil         => ""
        case Variable(b) => "(" + b + ")"
        case Lit(b)      => if b then "(true)" else "(false)"
        case Expr(op, l, r) => {
          if op == Op.Neg then { "(!" + l.toString() + ")" }
          else { "(" + l.toString() + op.toStr() + r.toString() + ")" }
        }
    run()
  def eval(e: BoolExpr = this): Boolean =
    def run(expr: BoolExpr = e): Boolean =
      expr match
        case Lit(b)               => if b then true else false
        case Expr(Op.And, l, r)   => if run(l) && run(r) then true else false
        case Expr(Op.Or, l, r)    => if run(l) || run(r) then true else false
        case Expr(Op.Neg, l, Nil) => if run(l) then false else true
    run()
  def getVariables(expr: BoolExpr = this): List[String] =
    var vars = Set[String]()
    def run(expression: BoolExpr = expr): Any =
      expression match
        case Variable(b) => vars = vars + b
        case Expr(op, l, r) => {
          run(l)
          run(r)
        }
        case _ =>
    run()
    vars.toList

  def setVariable(
                   variable: String,
                   value: Boolean
                 ): BoolExpr =
    def run(expression: BoolExpr = this): BoolExpr =
      expression match
        case Expr(op, l, r) => Expr(op, run(l), run(r))
        case Lit(b)         => Lit(b)
        case Nil            => Nil
        case Variable(b)    => if b == variable then Lit(value) else Variable(b)
    run()

  def getValues(
                 right: BoolExpr,
                 left: BoolExpr = this
               ): Set[(String, Boolean)] =
    var values = Set[(String, Boolean)]()
    def run(
             expr: BoolExpr = left,
             init: BoolExpr = right
           ): Set[(String, Boolean)] =
      (expr, init) match
        case (Nil, Nil)       => values
        case (Lit(a), Lit(b)) => values
        case (Variable(a), Lit(b)) => {
          values = values + ((a, b))
          values
        }
        case (Expr(a, b, c), Expr(d, e, f)) => {
          run(b, e)
          run(c, f)
        }
    run()

  def varToBool(expr: BoolExpr = this): List[Set[(String, Boolean)]] =
    var result = List[Set[(String, Boolean)]]()
    var bools = Set[BoolExpr](expr)
    def run(
             expression: BoolExpr = expr,
             variables: List[String] = expr.getVariables(),
             cnt: Int = 0
           ): Set[BoolExpr] =
      var bool = Set[BoolExpr]()
      if cnt < variables.length then {
        bools.foreach(e => bool += e.setVariable(variables(cnt), true))
        bools.foreach(e => bool += e.setVariable(variables(cnt), false))
        bools = bool
        run(expr, expr.getVariables(), cnt + 1)
      } else bools
    run()
    bools.foreach(myexpr => {
      if myexpr.eval() == true then result = result :+ expr.getValues(myexpr)
    })
    result

object BoolExpr {
  def apply(value: Boolean): BoolExpr =
    Lit(value)
  def apply(value: String): BoolExpr =
    Variable(value)
  def apply(op: Op, l: BoolExpr): BoolExpr =
    Expr(op, l, Nil)
  def apply(op: Op, l: String): BoolExpr =
    Expr(op, BoolExpr(l), Nil)
  def apply(op: Op, l: Boolean): BoolExpr =
    Expr(op, BoolExpr(l), Nil)
  def apply(op: Op, l: BoolExpr, r: BoolExpr): BoolExpr =
    Expr(op, l, r)
  def apply(op: Op, l: Boolean, r: BoolExpr): BoolExpr =
    Expr(op, BoolExpr(l), r)
  def apply(op: Op, l: BoolExpr, r: Boolean): BoolExpr =
    Expr(op, l, BoolExpr(r))
  def apply(op: Op, l: Boolean, r: Boolean): BoolExpr =
    Expr(op, BoolExpr(l), BoolExpr(r))
  def apply(op: Op, l: String, r: BoolExpr): BoolExpr =
    Expr(op, BoolExpr(l), r)
  def apply(op: Op, l: BoolExpr, r: String): BoolExpr =
    Expr(op, l, BoolExpr(r))
  def apply(op: Op, l: String, r: String): BoolExpr =
    Expr(op, BoolExpr(l), BoolExpr(r))
  def apply(op: Op, l: String, r: Boolean): BoolExpr =
    Expr(op, BoolExpr(l), BoolExpr(r))
  def apply(op: Op, l: Boolean, r: String): BoolExpr =
    Expr(op, BoolExpr(l), BoolExpr(r))
}

@main def run() =
  var first = BoolExpr(Op.And, BoolExpr(Op.Or, true, "x"),"y")
  println(first.varToBool())





