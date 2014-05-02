package highlight.color

object ColorProperty {
  import scala.math._
  // Create color properties
  def lightness(c: Color): Double = c.toLab._1/100.0
  
  def saturation(c: Color): Double = {
    val lab = c.toLab; 
    sqrt(lab._2 * lab._2 + lab._3 * lab._3)/sqrt(lab._1*lab._1 + lab._2*lab._2+lab._3*lab._3)
  }
  
  def hue(c: Color): Double = {
    val lab = c.toLab
    atan2(lab._3, lab._2)
  }

  def perceptualDifference(c1: Color, c2: Color): Double = {
    val lab1 = c1.toLab; val lab2 = c2.toLab; 
    val lab = (lab1._1 - lab2._1, lab1._2 - lab2._2, lab1._3 - lab2._3)
    sqrt(lab._1*lab._1 + lab._2*lab._2 + lab._3*lab._3) / 100.
  }
  def perceptualDiffTo(ref:Color)(c:Color): Double = perceptualDifference(ref, c)
  def relativeLightness(c1: Color, c2: Color): Double = {
    abs(lightness(c1) - lightness(c2))
  }
  
  def relativeSaturation(c1: Color, c2: Color): Double = {
    abs(saturation(c1) - saturation(c2))
  }

  def chromaticDifference(c1: Color, c2: Color): Double = {
    val lab1 = c1.toLab; val lab2 = c2.toLab; 
    val lab = (lab1._1 - lab2._1, lab1._2 - lab2._2, lab1._3 - lab2._3)
    sqrt(lab._2 * lab._2 + lab._3 * lab._3)/sqrt(lab._1*lab._1 + lab._2*lab._2+lab._3*lab._3)
  }
}