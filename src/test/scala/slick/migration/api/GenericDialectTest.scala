package slick.migration.api

import slick.jdbc.H2Profile
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class GenericDialectTest extends AnyFunSuite with Matchers {
  test("detect dialect for custom drivers") {
    object CustomH2Profile extends H2Profile

    GenericDialect(H2Profile) shouldBe a [H2Dialect]
    GenericDialect(CustomH2Profile) shouldBe a [H2Dialect]
  }
}
