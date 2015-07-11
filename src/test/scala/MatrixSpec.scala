
import pedrocorral.Matrix
import org.scalatest._
import Matchers._

class MatrixSpec extends FlatSpec {

  "Square matrix" should "be created" in {
    Matrix(
      1, 2,
      3, 4
    ).sizes shouldBe (2,2)
  }

  "Non square matrix" should "be rejected" in {
    intercept[Error] {
      Matrix(
        1, 2, 3,
        4, 5, 6
      )
    }
  }

  "Sample matrix" should "have a non zero determinant" in {
    Matrix(
      1,-3,
      1, 2
    ).determinant shouldBe 5
  }

  "Matrices" should "be different" in {
    assert(
      ( Matrix(
        1, 2,
        3, 4
      ) == Matrix(
        1, 2,
        3, 5
      ) ) === false
    )
  }

  "Matrices" should "be different (too)" in {
    assert(
      ( Matrix(
        1, 2,
        3, 4
      ) == Matrix(
        1, 2, 3,
        4, 5, 6,
        7, 8, 9
      ) ) === false
    )
  }

  "Matrices" should "be equal" in {
    assert( Matrix( 1, 2, 4, 8 ) == Matrix( 1, 2, 4, 8 ) )
  }

  "A matrix" should "have some minors" in {
    val M = Matrix(
      -2, 2,-3,
      -1, 1, 3,
       2, 0,-1
    )
    assert( M.minor(1,1) == Matrix( 1,3,0,-1 ) )
    assert( M.minor(2,2) == Matrix( -2,-3,2,-1 ) )
    assert( M.minor(1,3) == Matrix( -1,1,2,0 ) )
  }

  "A matrix" should "calculate its determinant" in {
    Matrix(
      -2, 2,-3,
      -1, 1, 3,
       2, 0,-1
    ).determinant shouldBe 18
  }

}

