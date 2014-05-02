package highlight.color

object Function {
  import scala.math._
  def CNDF(xInput:Double): Double = {
    var x = xInput
    val neg: Int = if (x < 0) 1 else 0;
    if ( neg == 1) 
        x = x * -1;

    var k:Double = (1 / ( 1 + 0.2316419 * x));
    var y:Double = (((( 1.330274429 * k - 1.821255978) * k + 1.781477937) *
                   k - 0.356563782) * k + 0.319381530) * k;
    y = 1.0 - 0.398942280401 * exp(-0.5 * x * x) * y;

    return (1.0 - neg) * y + neg * (1.0 - y);    
  }
}