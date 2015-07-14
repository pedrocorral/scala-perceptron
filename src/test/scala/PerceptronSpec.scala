
import pedrocorral.Perceptron
import org.scalatest._
import Matchers._

class PerceptronSpec extends FlatSpec {

  "σ(Sigma)" should "work properly" in {
    Perceptron.σ(0) shouldBe 0.5
    Perceptron.σ(1) should be > 0.7
    Perceptron.σ(-1) should be < 0.27
  }

  "Perceptron" should "act as a logic operator (and)" in {
    val p_and = Perceptron( 1.0 :: 1.0 :: Nil, -1.0 )
    p_and.bool( 1.0 :: 1.0 :: Nil ) shouldBe true
    p_and.bool( 0.0 :: 1.0 :: Nil ) shouldBe false
    p_and.bool( 1.0 :: 0.0 :: Nil ) shouldBe false
    p_and.bool( 0.0 :: 0.0 :: Nil ) shouldBe false
  }

  "Perceptron" should "act as a logic operator (or)" in {
    val p_or = Perceptron( 0.5 :: 0.5 :: Nil, 0.0 )
    p_or.bool( 1.0 :: 1.0 :: Nil ) shouldBe true
    p_or.bool( 0.0 :: 1.0 :: Nil ) shouldBe true
    p_or.bool( 1.0 :: 0.0 :: Nil ) shouldBe true
    p_or.bool( 0.0 :: 0.0 :: Nil ) shouldBe false
  }

  "Perceptron" should "be created" in {
    Perceptron.functionalTraining(
      1.0 :: 0.0 :: 1.0 :: Nil,
      0.0 :: 0.0 :: 0.0 :: Nil
    ) shouldBe defined
  }

  "Perceptron" should "be linearly independent" in {
    Perceptron.functionalTraining(
      1.0 :: 0.0 :: 1.0 :: Nil,
      0.0 :: 0.0 :: 1.0 :: Nil
    ) shouldBe empty
  }

  "Perceptron" should "be trained as logic operator (and)" in {
    val p_and = Perceptron.functionalTraining(
      1.0 :: 1.0 :: 1.0 :: Nil,
      0.0 :: 0.0 :: 1.0 :: Nil,
      0.0 :: 1.0 :: 0.0 :: Nil,
      0.0 :: 0.0 :: 0.0 :: Nil
    )
    p_and shouldBe defined
    p_and.get.bool( 1.0 :: 1.0 :: Nil ) shouldBe true
    p_and.get.bool( 0.0 :: 1.0 :: Nil ) shouldBe false
    p_and.get.bool( 1.0 :: 0.0 :: Nil ) shouldBe false
    p_and.get.bool( 0.0 :: 0.0 :: Nil ) shouldBe false
  }

  "Perceptron" should "be trained as logic operator (or)" in {
    val p_or = Perceptron.functionalTraining(
      1.0 :: 1.0 :: 1.0 :: Nil,
      1.0 :: 0.0 :: 1.0 :: Nil,
      1.0 :: 1.0 :: 0.0 :: Nil,
      0.0 :: 0.0 :: 0.0 :: Nil
    )
    p_or shouldBe defined
    p_or.get.bool( 1.0 :: 1.0 :: Nil ) shouldBe true
    p_or.get.bool( 0.0 :: 1.0 :: Nil ) shouldBe true
    p_or.get.bool( 1.0 :: 0.0 :: Nil ) shouldBe true
    p_or.get.bool( 0.0 :: 0.0 :: Nil ) shouldBe false
  }

  "Perceptron" should "be linear predictor. In other words, cannot act as a XOR operand" in {
    Perceptron.functionalTraining(
      0.0 :: 1.0 :: 1.0 :: Nil,
      1.0 :: 0.0 :: 1.0 :: Nil,
      1.0 :: 1.0 :: 0.0 :: Nil,
      0.0 :: 0.0 :: 0.0 :: Nil
    ) shouldBe empty
  }

}
