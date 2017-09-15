package Chapter7

import org.scalatest._


class PolynomialsSuite extends FunSuite{
  trait Polys {
    val twoAndThree = List(2.0, 3)
    val twoAndNegThree = List(2.0, -3.0)
    val fourAndFive = List(4.0, 5)
    val twoThreeFourFiveProduct = List(8.0, 22.0, 15)

    val threeUp = List(1.0, 2.0, 3.0)
    val threeDown = List(5.0, 3.0, 1.0)
    val threesSum = List(6.0, 5.0, 4.0)

    val fiveDown = List(7.0, 4.0, 2.0, 1.0, 1.0)
    val threeFiveSum = List(7.0, 4.0, 3.0, 3.0, 4.0)

  }

  test("simple addition: (x^2 + 2x + 3) + (5x^2 + 3x + 1) = 6x^2 + 5x + 4") {
    new Polys {
      assert(Polynomials.add(threeUp, threeDown) == threesSum)
    }
  }

  test("different polynomial length addition: (x^2 + 2x + 3) + (7x^4 + 4x^3 + 2x^2 + x + 1) = 7x^4 + 4x^3 + 3x^2 + 3x + 4") {
    new Polys {
      assert(Polynomials.add(threeUp, fiveDown) == threeFiveSum)
    }
  }

  test("simple subtraction: (6x^2 + 5x + 4) - (5x^2 + 3x + 1) = x^2 + 2x + 3"){
    new Polys {
      assert(Polynomials.subtract(threesSum, threeDown) == threeUp)
    }
  }

  test("printPolynomial prints pretty polys like x^2 + 2x + 3") {
    new Polys {
      assert(Polynomials.polynomialAsString(threeUp) == "x^2 + 2x + 3")
    }
  }

  test("simple multiplication: (2x + 3)(4x + 5) = 8x^2 + 22x + 15") {
    new Polys {
      assert(Polynomials.multiply(twoAndThree, fourAndFive) == twoThreeFourFiveProduct)
    }
  }

  test("different Polynomial length multiplication: (2x + 3)(x^2 + 2x + 3) = 2x^3 + 7x^2 + 12x + 9") {
    new Polys {
      assert(Polynomials.multiply(twoAndThree, threeUp) == List(2.0, 7.0, 12.0, 9.0))
    }
  }

  test("multiplication with zero terms: (2x + 3)(2x - 3) = 4x^2 + 0.0x - 9") {
    new Polys {
      assert(Polynomials.multiply(twoAndThree, twoAndNegThree) == List(4.0, 0.0, -9.0))
    }
  }
}
