import language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
}

sealed trait Exp[A]
final case class IntValue[A](v: Int) extends Exp[A]
final case class DecValue[A](v: Double) extends Exp[A]
final case class Sum[A](exp1: A, exp2: A) extends Exp[A]
final case class Multiply[A](exp1: A, exp2: A) extends Exp[A]
final case class Divide[A](exp1: A, exp2: A) extends Exp[A]
final case class Square[A](exp: A) extends Exp[A]

object Exp {
  implicit val expFunctor: Functor[Exp] = new Functor[Exp] {
    def map[A, B](fa: Exp[A])(f: (A) => B): Exp[B] = fa match {
      case IntValue(v) => IntValue(v)
      case DecValue(v) => DecValue(v)
      case Sum(exp1, exp2) => Sum(f(exp1), f(exp2))
      case Multiply(exp1, exp2) => Multiply(f(exp1), f(exp2))
      case Divide(exp1, exp2) => Divide(f(exp1), f(exp2))
      case Square(exp) => Square(f(exp))
    }
  }
}

case class Fix[F[_]](unFix: F[Fix[F]])


object FixpointPlayground extends App {

  val expr: Fix[Exp] = Fix(Sum(Fix(IntValue(2)), Fix(DecValue(3.5))))

  println(expr)

  // catamorphisms

  type Algebra[F[_], A] = F[A] => A

  val evaluate: Algebra[Exp, Double] = {
    case IntValue(v) => v.toDouble
    case DecValue(v) => v
    case Sum(exp1, exp2) => exp1 + exp2
    case Multiply(exp1, exp2) => exp1 * exp2
    case Divide(exp1, exp2) => exp1 / exp2
    case Square(exp) => exp * exp
  }

  val mkStr: Algebra[Exp, String] = {
    case IntValue(v) => v.toString
    case DecValue(v) => v.toString
    case Sum(exp1, exp2) => s"$exp1 + $exp2"
    case Multiply(exp1, exp2) => s"$exp1 * $exp2"
    case Divide(exp1, exp2) => s"$exp1 / $exp2"
    case Square(exp) => s"($exp)^2"
  }

  def cata[F[_]: Functor, A](algebra: Algebra[F, A], x: Fix[F]): A = {
    val functorInst = implicitly[Functor[F]]
    algebra(functorInst.map(x.unFix)(cata(algebra, _)))
  }

  val dblResult = cata(evaluate, expr)
  val strResult = cata(mkStr, expr)

  println(strResult + " = " + dblResult)

  // anamorphisms

  type Coalgebra[F[_], A] = A => F[A]

  val divisors: Coalgebra[Exp, Int] = {
    case n if n % 2 == 0 && n != 2 => Multiply(2, n / 2)
    case n => IntValue(n)
  }

  def ana[F[_]: Functor, A](coalgebra: Coalgebra[F, A], x: A): Fix[F] = {
    val functorInst = implicitly[Functor[F]]
    Fix(functorInst.map(coalgebra(x))(ana(coalgebra, _)))
  }

  val expr2 = ana(divisors, 12)

  println(expr2)

  // hylomorphism

  def hylo[F[_]: Functor, A, B](coalgebra: Coalgebra[F, A], algebra: Algebra[F, B], a: A): B = {
    cata(algebra, ana(coalgebra, a))
  }

  val expr2Str = hylo(divisors, mkStr, 12)

  println(s"12 = $expr2Str")
}
