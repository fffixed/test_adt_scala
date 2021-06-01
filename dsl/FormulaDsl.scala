package dsl

import scala.language.implicitConversions

object FormulaDsl {

  type Parameter = (String, Double)

  implicit class FormulaDouble(x: Double) {
    def $: Formula = Formula.Const(x)
  }

  implicit class FormulaString(x: String) {
    def $: Formula = Formula.Param(x)
  }

  implicit class FormulaHelper(val sc: StringContext) extends AnyVal {
    def $(args: Any*): Formula = Formula.Param(sc.parts.head)
  }

  implicit def doubleToFormula(x: Double): Formula = Formula.Const(x)


  sealed trait Formula extends Product with Serializable {

    def apply(param: Parameter*): Double = eval(param: _*)

    def +(x: Formula): Formula = Formula.CalcAddition(this, x)

    def -(x: Formula): Formula = Formula.CalcSubtraction(this, x)

    def *(x: Formula): Formula = Formula.CalcProduct(this, x)

    def /(x: Formula): Formula = Formula.CalcDivision(this, x)

    protected def eval(param: Parameter*): Double

    protected def print: String

    override def toString: String = print

  }

  object Formula {

    def apply(param: Parameter*): Start.type = Start

    case object Start extends Formula {
      override protected def eval(param: Parameter*): Double = 0
      override protected def print: String = ""
      def :=(x: Formula): Formula = CalcAddition(Start, x)
    }

    case class Const(value: Double) extends Formula {
      override protected def eval(param: Parameter*): Double = value
      override protected def print: String = value.toString
    }

    case class Param(name: String) extends Formula {
      override protected def eval(param: Parameter*): Double = param.toMap.getOrElse(name, throw new Exception(s"Parameter '$name' not passed!"))
      override protected def print: String = name
    }

    case class CalcAddition(a: Formula, b: Formula) extends Formula {
      override protected def eval(param: Parameter*): Double = a.eval(param: _*) + b.eval(param: _*)
      override protected def print: String = if (a.isInstanceOf[Formula.Start.type]) b.print else s"${a.print} + ${b.print}"
    }

    case class CalcSubtraction(a: Formula, b: Formula) extends Formula {
      override protected def eval(param: Parameter*): Double = a.eval(param: _*) - b.eval(param: _*)
      override protected def print: String = s"${a.print} - ${b.print}"
    }

    case class CalcProduct(a: Formula, b: Formula) extends Formula {
      override protected def eval(param: Parameter*): Double = a.eval(param: _*) * b.eval(param: _*)
      override protected def print: String = s"${printA} * ${printB}"

      private def printA: String = a match {
        case _: CalcAddition | _: CalcSubtraction => s"(${a.print})"
        case _ => s"${a.print}"
      }

      private def printB: String = b match {
        case _: Const | _: Param => b.print
        case _ => s"(${b.print})"
      }
    }

    case class CalcDivision(a: Formula, b: Formula) extends Formula {
      override protected def eval(param: Parameter*): Double = a.eval(param: _*) / b.eval(param: _*)

      override protected def print: String = s"${printA} / ${printB}"

      private def printA: String = a match {
        case _: CalcAddition | _: CalcSubtraction => s"(${a.print})"
        case _ => s"${a.print}"
      }

      private def printB: String = b match {
        case _: Const | _: Param => b.print
        case _ => s"(${b.print})"
      }
    }

  }

}
