
import cc.factorie.variable._
import cc.factorie._
import cc.factorie.la._
import cc.factorie.model._
import cc.factorie.infer._

package highlight.color {
  class ColorVariable(v: Color) extends RefVariable[Color](v) 
  // Create factors
  class UnaryColorFactor(c: ColorVariable, m: Color => Double, f: Double => Double) extends Factor1(c) {
    def score(v: Color): Double = {
      f(m(v))
    }
    override def factorName = "UnaryColorFactor"
  }
  class PairwiseColorFactor(c1: ColorVariable, c2: ColorVariable, m: (Color, Color) => Double, f: Double => Double) extends Factor2(c1, c2) {
    def score(v1: Color, v2: Color): Double = {
      f(m(v1, v2))
    }
    override def factorName = "PairwiseColorFactor"
  }
}