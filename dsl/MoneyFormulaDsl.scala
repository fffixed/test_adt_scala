package dsl

object MoneyFormulaDsl {

  sealed trait AlmostMoney {
    val amount: Double

    def +(x: AlmostMoney): AlmostMoney = this match {
      case Money(am) =>
        x match {
          case Money(bm) => Money(am + bm)
          case _: Number => throw new Exception("Чёт не получается к деньгам насыпать цифр")
        }
      case Number(an) =>
        x match {
          case Number(bn) => Number(an + bn)
          case _: Money => throw new Exception("Чёт не получается к числу прилепясить денег")
        }
    }

    def -(x: AlmostMoney): AlmostMoney = this match {
      case Money(am) =>
        x match {
          case Money(bm) => Money(am - bm)
          case _: Number => throw new Exception("Вычесть из денег число не получится")
        }
      case Number(an) =>
        x match {
          case Number(bn) => Number(an - bn)
          case _: Money => throw new Exception("Вычесть из числа денег не получится")
        }
    }

    def *(x: AlmostMoney): AlmostMoney = this match {
      case Money(am) =>
        x match {
          case Number(bn) => Money(am * bn)
          case _: Money => throw new Exception("Квадратные деньги отстой, непрактично")
        }
      case Number(an) =>
        x match {
          case Money(bm) => Money(an * bm)
          case Number(bn) => Number(an * bn)
        }
    }

    def /(x: AlmostMoney): AlmostMoney = this match {
      case Money(am) =>
        x match {
          case Number(bn) => Money(am / bn)
          case Money(bm) => Number(am / bm)
        }
      case Number(an) =>
        x match {
          case Number(bn) => Number(an / bn)
          case _: Money => throw new Exception("Число на деньги не делится, только наоборот")
        }
    }

  }

  case class Money(amount: Double) extends AlmostMoney {
    override def toString: String = "$" + amount.toString
  }
  case class Number(amount: Double) extends AlmostMoney {
    override def toString: String = amount.toString
  }



  object ImplicitFormulaHelper {
    implicit def doubleToFormula(x: Double): MoneyFormula = MoneyFormula.Const(Number(x))
    implicit def stringToFormula(x: String): MoneyFormula = MoneyFormula.Param(x)
    implicit def almostMoneyToFormula(x: AlmostMoney): MoneyFormula = MoneyFormula.Const(x)

    implicit class MoneyFormulaOps(mf: MoneyFormula) {
      import MoneyFormula._
      def +(x: AlmostMoney): MoneyFormula = CalcAddition(mf, MoneyFormula(x))
      def -(x: AlmostMoney): MoneyFormula = CalcSubtraction(mf, MoneyFormula(x))
      def *(x: AlmostMoney): MoneyFormula = CalcProduct(mf, MoneyFormula(x))
      def /(x: AlmostMoney): MoneyFormula = CalcDivision(mf, MoneyFormula(x))
    }


  }

  implicit class AlmostMoneyOps(x: AlmostMoney) {
    def mf: MoneyFormula = MoneyFormula.Const(x)
  }

  implicit class StringOps(x: String) {
    def mf: MoneyFormula = MoneyFormula.Param(x)
  }

  implicit class DoubleOps(x: Double) {
    def $: AlmostMoney = Money(x)
    def n: AlmostMoney = Number(x)
  }



  object F {
    def apply(x: AlmostMoney): MoneyFormula = MoneyFormula(x)
    def apply(x: Double): MoneyFormula = MoneyFormula(x.n)
    def apply(x: String): MoneyFormula = MoneyFormula(x)
  }

  type Parameter = (String, AlmostMoney)

  sealed trait MoneyFormula extends Product with Serializable {
    import MoneyFormula._
    protected def eval(param: Parameter*): AlmostMoney
    protected def print: String
    override def toString: String = print
    def apply(param: Parameter*): AlmostMoney = eval(param: _*)

    def +(x: MoneyFormula): MoneyFormula = CalcAddition(this, x)
    def -(x: MoneyFormula): MoneyFormula = CalcSubtraction(this, x)
    def *(x: MoneyFormula): MoneyFormula = CalcProduct(this, x)
    def /(x: MoneyFormula): MoneyFormula = CalcDivision(this, x)
  }


  object MoneyFormula {

    def apply(x: AlmostMoney): MoneyFormula = MoneyFormula.Const(x)
    def apply(x: Double): MoneyFormula = MoneyFormula.Const(x.n)
    def apply(x: String): MoneyFormula = MoneyFormula.Param(x)

    case class Const(value: AlmostMoney) extends MoneyFormula {
      override protected def eval(param: Parameter*): AlmostMoney = value
      override protected def print: String = value.toString
    }

    case class Param(name: String) extends MoneyFormula {
      override protected def eval(param: Parameter*): AlmostMoney = {
        param.toMap.getOrElse(name, throw new Exception(s"Параметр '$name' не передан"))
      }
      override protected def print: String = name
    }

    case class CalcAddition(a: MoneyFormula, b: MoneyFormula) extends MoneyFormula {
      override protected def eval(param: Parameter*): AlmostMoney = a.eval(param:_*) + b.eval(param:_*)
      override protected def print: String = s"${a.print} + ${b.print}"
    }

    case class CalcSubtraction(a: MoneyFormula, b: MoneyFormula) extends MoneyFormula {
      override protected def eval(param: Parameter*): AlmostMoney = a.eval(param:_*) - b.eval(param:_*)
      override protected def print: String = s"${a.print} - ${b.print}"
    }

    case class CalcProduct(a: MoneyFormula, b: MoneyFormula) extends MoneyFormula {
      override protected def eval(param: Parameter*): AlmostMoney = a.eval(param:_*) * b.eval(param:_*)
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

    case class CalcDivision(a: MoneyFormula, b: MoneyFormula) extends MoneyFormula {
      override protected def eval(param: Parameter*): AlmostMoney = a.eval(param:_*) / b.eval(param:_*)
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
