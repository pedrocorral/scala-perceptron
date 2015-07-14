
import pedrocorral.Perpectron
import org.scalatest._
import Matchers._

class PerceptronSpec extends FlatSpec {

  "Perceptron" should "be created" in {
    Perceptron(
      1 :: 0 :: 1 :: Nil,
      0 :: 0 :: 0 :: Nil
    ) should be defined
  }

  "Perceptron" should "be linearly independent" in {
    Perceptron(
      1 :: 0 :: 1 :: Nil,
      0 :: 0 :: 1 :: Nil
    ) should be None
  }

}
