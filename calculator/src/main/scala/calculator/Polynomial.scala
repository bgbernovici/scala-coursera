package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal[Double] {
      Math.pow(b(), 2) - 4 * a() * c()
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal[Set[Double]] {
      val cachedDelta = computeDelta(a, b, c)()
      if cachedDelta < 0 then 
        Set.empty 
      else
        Set(
          (-b() + Math.sqrt(cachedDelta))/(2*a()), 
          (-b() - Math.sqrt(cachedDelta))/(2*a())
        )
    }
