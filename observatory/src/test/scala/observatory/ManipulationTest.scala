package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait ManipulationTest extends FunSuite with Checkers {

  test("grid should resolve to 64800 elements") {
    val data = Seq(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0)
    )
    val resolver = Manipulation.makeGrid(data)
//    assert(resolver(GridLocation(180, 90)) === 0.0)
  }

}