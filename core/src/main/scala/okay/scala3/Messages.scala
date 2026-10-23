package okay.scala3

import scala.quoted.*

/**
 * THE COMPILE ERRORS of a missing effect: the one macro here, for what given search cannot say — the effect and the
 * row as they are written, `State % Int` and `Reader % String + State % Int`, where Scala would print the type
 * lambdas they are, or nothing at all under a context bound.
 */
object Messages:
  def notIn[E[_]: Type, R[_]: Type](using Quotes): Expr[Nothing] =
    import quotes.reflect.*
    report.errorAndAbort(s"the effect ${written(TypeRepr.of[E])} is not in the row ${written(TypeRepr.of[R])}")

  /** a row or an effect as it is written: `+` and `%` for the lambdas they make, `Pure` for `Void` */
  def written(using Quotes)(t: quotes.reflect.TypeRepr): String =
    import quotes.reflect.*
    def of(t: TypeRepr, nested: Boolean): String = t.dealias match
      case TypeLambda(List(x), _, AppliedType(f, args)) =>
        val parameter = args.lastOption.collect { case p: ParamRef => p }
        (f.typeSymbol.fullName, args) match
          case ("okay.scala3.Sum", List(r, e, _)) if parameter.isDefined =>
            val sum = s"${of(r, nested = false)} + ${of(e, nested = true)}"
            if nested then s"($sum)" else sum
          case (_, List(s, _)) if parameter.isDefined => s"${f.typeSymbol.name} % ${s.show(using Printer.TypeReprShortCode)}"
          case (_, List(_)) if parameter.isDefined => f.typeSymbol.name
          case _ => t.show(using Printer.TypeReprShortCode)
      case tpe if tpe.typeSymbol.fullName == "okay.scala3.Void" => "Pure"
      case tpe => tpe.typeSymbol.name
    of(t, nested = false)
