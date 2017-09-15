package Chapter7

object Polynomials {

  type Polynomial = List[Double]

  def add(p1: Polynomial, p2: Polynomial): Polynomial = {
    def addAcc(accX: Polynomial, accY: Polynomial, accResult: Polynomial): Polynomial = (accX, accY) match {
      case (Nil, Nil) => accResult
      case (Nil, ys) => ys.reverse ++ accResult
      case (xs, Nil) => xs.reverse ++ accResult
      case (x::xs, y::ys) => addAcc(xs, ys, (x + y)::accResult)
    }
    addAcc(p1.reverse, p2.reverse, Nil)
  }

  def subtract(p1: Polynomial, p2: Polynomial): Polynomial = {
    add(p1, p2 map (_* -1.0))
  }

  /**
    * (Ax2 + Bx + C) * (Ex2 + Fx + G) =
    * (Ax2*Ex2 + Ax2*Fx + Ax2*G + Bx*Ex2 + Bx*Fx + Bx*G + C*Ex2 + C*Fx + C*G) =
    * (AEx4 + (AG+BE)x3 + (AG+BF+E)x2 + (BG+CF)x + CG)
    * @param p1
    * @param p2
    * @return
    */
  def multiply(p1: Polynomial, p2: Polynomial): Polynomial = {
    zipWithExponent(p1)
      .view
      .flatMap(t => zipWithExponent(p2).map(t2 => (t._1*t2._1, t._2+t2._2))) //cartesian product
      .groupBy(t => t._2) //grouped by exponent, so we can add them
      .map(t => (t._2.map(termWithSameExp => termWithSameExp._1).sum, t._1)) //add terms of like exponents
      .toList.sortBy(-_._2) // sort by exp descending since that's the defintion of Polynomial type
      .map(_._1)
  }

  def zipWithExponent(p: Polynomial): Seq[(Double, Int)] = {
    p.zipWithIndex.map(t => (t._1, p.length - 1 - t._2))
  }

  def polynomialAsString(p: Polynomial): String = {
    p.zipWithIndex
      .collect{case (c, index) if c != 0 => simplifyPolyTerm(c, p.length - 1 - index)}
      .mkString(" + ")
  }

  def simplifyPolyTerm(coefficient: Double, exponent: Int): String = exponent match {
    case 0 => simplifyPolyCoefficient(coefficient)
    case 1 => simplifyPolyCoefficient(coefficient) + "x"
    case _ => simplifyPolyCoefficient(coefficient) + "x^" + exponent
  }

  def simplifyPolyCoefficient(d: Double): String = {
    if(d == 1.0) ""
    else d.toString.replaceAll("\\.*0+$", "")
  }
}
