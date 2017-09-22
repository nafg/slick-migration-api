package slick.migration.api

import org.scalatest.{FunSuite, Matchers}
import slick.jdbc.H2Profile

class GenericDialectTest extends FunSuite with Matchers {
  test("detect dialect for custom drivers") {
    object CustomH2Profile extends H2Profile

    GenericDialect(H2Profile) shouldBe a [H2Dialect]
    GenericDialect(CustomH2Profile) shouldBe a [H2Dialect]
  }
}
