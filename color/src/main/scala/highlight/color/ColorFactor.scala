
import scala.collection.mutable._  

case class customException(smth:String) extends Exception(smth)

package highlight.color {
object ColorFactor extends App {
  /*************************************************************************************************/
  // Load Raw Data, and compute necessary data structure
  import spray.json._
  import DefaultJsonProtocol._ // !!! IMPORTANT, else `convertTo` and `toJson` won't work correctly  
  import scala.io.Source
  val source = Source.fromFile("../out.json")
  val jString = source.mkString
  source.close()  
  //println(jString)
  
  val jsAST = jString.parseJson
  val unaryMap = jsAST.asJsObject.getFields("unary")(0).asJsObject.fields
  val pairMap = jsAST.asJsObject.getFields("pair")(0).asJsObject.fields
  val tokenKeys = unaryMap.keys
  
  
  def parent(v:String): String = {  
    if (v == "") throw new customException("Null Node")
    v.split("""\.""").dropRight(1).mkString(".")
  }
  def addTo(m: Set[String], v: String): Unit = {
    if (v == "Token" || m.contains(v)) return // Token denotes background
    m += v
    addTo(m, parent(v))
  }
  
  val colorKeys = SortedSet[String]()
  tokenKeys.map(addTo(colorKeys, _))
  //println(colorKeys)
  
  /*************************************************************************************************/
  // Create factor graph
  import cc.factorie._
  import cc.factorie.la._
  import cc.factorie.variable._
  import cc.factorie.model._
  import scala.math._
  import cc.factorie.infer._
  // Create variables
  class Color(var v: (Double, Double, Double)) {
    override def toString() = v.toString()
  }
  final var randomGenerator = new scala.util.Random
  def randomColor(): Color = new Color((randomGenerator.nextDouble,randomGenerator.nextDouble,randomGenerator.nextDouble))
  class ColorVariable(v: Color) extends RefVariable[Color](v) 
  
  val colorVars =  Map[String, ColorVariable]()
  colorKeys.map(colorVars += _ -> new ColorVariable(randomColor()))
  val colorSeqOfVars = colorVars.map{case (k,v) => v}(collection.breakOut) // without background
  colorVars += "Token" -> new ColorVariable(new Color((0,0,0))) // background
  
  
  // Create factors
  class PairwiseColorFactor(c1: ColorVariable, c2: ColorVariable, f: Double => Double) extends Factor2(c1, c2) {
    def score(v1: Color, v2: Color): Double = {
      //val sqr: Double => Double = x => x*x
      val d = abs(v1.v._1 - v2.v._1) + abs(v1.v._2 - v2.v._2) + abs(v1.v._3 - v2.v._3)
      f(d)
    }
    override def factorName = "ColorFactor"
  }
  
  
  // Create model
  val m1 = new ItemizedModel  
  colorKeys.map({key =>
      val pKey = parent(key)
  	  if (key != "Token" || pKey != "Token") {
  	    m1 ++= new PairwiseColorFactor(colorVars(key), colorVars(pKey), x => sin(x * 2.0))
  	  }
      if (key != "Token") {
        m1 ++= new PairwiseColorFactor(colorVars(key), colorVars("Token"), x => -(x-3)*(x-3)/5)
      }
  })
  
  
  println(m1.currentScore(colorSeqOfVars))
  
  // Create Sampler
  final var randomGenerator2 = new scala.util.Random
  class MHColorSampler(model: Model) extends MHSampler[ColorVariable](model)(randomGenerator2) {
    def propose(context:ColorVariable)(implicit d:DiffList) : Double = {
      val origScore = model.currentScore(context)
      context.set(randomColor())(d)
      val currScore = model.currentScore(context)
      currScore - origScore
    }
  }
  
  val sampler = new MHColorSampler(m1)
  sampler.processAll(colorSeqOfVars, 100)
  println(m1.currentScore(colorSeqOfVars))
  println(colorVars("Token"))
  
  
  
}
}