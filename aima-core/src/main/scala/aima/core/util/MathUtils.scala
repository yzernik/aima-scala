package aima.core.util

object MathUtils {
  final class Precision(val p: Double) extends AnyVal

  implicit val defaultPrecision: Precision = new Precision(1e-5)

  implicit class DoubleEqualsPrecision(val d: Double) extends AnyVal {
    @inline def leq(d2: Double)(implicit p: Precision): Boolean = d < d2 || ~=(d2)
    @inline def geq(d2: Double)(implicit p: Precision): Boolean = d > d2 || ~=(d2)
    @inline def ~=(d2: Double)(implicit p: Precision): Boolean = (d - d2).abs < p.p
    @inline def !~=(d2: Double)(implicit p: Precision): Boolean = !(~=(d2))(p)
  }
  implicit class FloatEqualsPrecision(val d: Float) extends AnyVal {
    @inline def leq(d2: Float)(implicit p: Precision): Boolean = d < d2 || ~=(d2)
    @inline def geq(d2: Float)(implicit p: Precision): Boolean = d > d2 || ~=(d2)
    @inline def ~=(d2: Float)(implicit p: Precision): Boolean = (d - d2).abs < p.p
    @inline def !~=(d2: Float)(implicit p: Precision): Boolean = !(~=(d2))(p)
  }
  @inline def within(a: Double, b: Double, c: Double): Boolean = ((a leq b) && (b leq c)) || ((a geq b) && (b geq c))
}
