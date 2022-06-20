package Lst
import munit.FunSuite
import BoolExpr.*
import BoolExpr._

class LstTest extends FunSuite {
  test("eval") {
    val expected = true
    val actual = BoolExpr(Op.And, true, BoolExpr(Op.Or, true, false)).eval()
    assertEquals(actual, expected)
  }
  test("eval") {
    val expected = false
    val actual = BoolExpr(Op.And, false, BoolExpr(Op.Or, true, false)).eval()
    assertEquals(actual, expected)
  }
  test("getVariables") {
    val expected = List("x", "y")
    val actual =
      BoolExpr(Op.And, "x", BoolExpr(Op.Or, "y", false)).getVariables()
    assertEquals(actual, expected)
  }
  test("setVariable") {
    val expected = BoolExpr(Op.And, true, BoolExpr(Op.Or, "y", false))
    val actual =
      BoolExpr(Op.And, "x", BoolExpr(Op.Or, "y", false)).setVariable("x", true)
    assertEquals(actual, expected)
  }
  test("getValues") {
    val not_assigned = BoolExpr(Op.And, "x", BoolExpr(Op.Or, "y", false))
    val assigned = BoolExpr(Op.And, true, BoolExpr(Op.Or, false, false))
    val expected = Set(("x", true), ("y", false))
    val actual = not_assigned.getValues(assigned)
    assertEquals(actual, expected)
  }
  test("varToBool") {
    val expected = Set(Set(("x", true), ("y", true)))
    val actual = BoolExpr(Op.And, "x", BoolExpr(Op.Or, "y", false)).varToBool()
    assertEquals(actual, expected)
  }
}
