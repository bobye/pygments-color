package highlight.color

case class Color(var v: (Double, Double, Double)) {
  // 0 <= r,g,b <= 1.0
    val r: Double = v._1
    val g: Double = v._2
    val b: Double = v._3
    
    override def toString() = v.toString()
    def toInt(): (Int, Int, Int) = ((r*255).toInt, (g*255).toInt, (b*255).toInt)
    def toLab(): (Double, Double, Double) = {
      // Reference: http://www.easyrgb.com/index.php?X=MATH
      // RGB to XYZ
      var var_R: Double = r
      var var_G: Double = g
      var var_B: Double = b
      
      import scala.math.pow
      if ( var_R > 0.04045 ) var_R = pow( ( var_R + 0.055 ) / 1.055, 2.4)
      else                   var_R = var_R / 12.92
      if ( var_G > 0.04045 ) var_G = pow( ( var_G + 0.055 ) / 1.055, 2.4)
      else                   var_G = var_G / 12.92
      if ( var_B > 0.04045 ) var_B = pow( ( var_B + 0.055 ) / 1.055, 2.4)
      else                   var_B = var_B / 12.92
      
      var_R = var_R * 100
      var_G = var_G * 100
      var_B = var_B * 100
      
      var X = var_R * 0.4124 + var_G * 0.3576 + var_B * 0.1805
      var Y = var_R * 0.2126 + var_G * 0.7152 + var_B * 0.0722
      var Z = var_R * 0.0193 + var_G * 0.1192 + var_B * 0.9505	
      // XYZ to Lab
      val ref_X = 95.047
      val ref_Y = 100.00
      val ref_Z = 108.883
      var var_X = X / ref_X          //ref_X =  95.047   Observer= 2°, Illuminant= D65
      var var_Y = Y / ref_Y          //ref_Y = 100.000
      var var_Z = Z / ref_Z          //ref_Z = 108.883
      
      if ( var_X > 0.008856 ) var_X = pow(var_X , 1.0/3.0 )
      else                    var_X = ( 7.787 * var_X ) + ( 16.0 / 116.0 )
      if ( var_Y > 0.008856 ) var_Y = pow(var_Y , 1.0/3.0 )
      else                    var_Y = ( 7.787 * var_Y ) + ( 16.0 / 116.0 )
      if ( var_Z > 0.008856 ) var_Z = pow(var_Z , 1.0/3.0 )
      else                    var_Z = ( 7.787 * var_Z ) + ( 16.0 / 116.0 )
      
      val CIE_L = ( 116 * var_Y ) - 16
      val CIE_a = 500 * ( var_X - var_Y )
      val CIE_b = 200 * ( var_Y - var_Z )

      (CIE_L, CIE_a, CIE_b)

    }
    def toYIQ(): (Double, Double, Double) = {
      val Y = 0.299 * r + 0.587 * g + 0.114 * b
      val I = 0.596 * r - 0.275 * g - 0.321 * b
      val Q = 0.212 * r - 0.523 * g + 0.311 * b
      (Y, I, Q)
    }
}

object Color {
  final def BLACK = Color((0.0,0.0,0.0))
  final def COLOR = Color((1.0,1.0,1.0))
}
