package highlight.color {
object ColorFactor extends App {
  /*************************************************************************************************/
  // Load Raw Data, and compute necessary data structure
  import scala.collection.mutable._  
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
  val pairKeys = pairMap.keys
  
  case class customException(smth:String) extends Exception(smth)
  
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
  
  val tokenFeats = Map[String, List[Double]]()
  tokenKeys.map(key => tokenFeats += key -> {
    val raw = unaryMap(key)
    raw.convertTo[List[Double]]
  })  
  //println(tokenFeats.toSeq.sortBy( - _._2(0)))
  
  val pairFeats = Map[String, List[Double]]()
  pairKeys.map(key => pairFeats += key -> {
    val raw = pairMap(key)
    val l = raw.convertTo[List[(Int, Int)]]
    val num = l.length
    val avg = l.reduce((v1,v2) => (v1._1 + v2._1, v1._2 + v2._2))
    List(num, avg._1.toDouble/num, avg._2.toDouble/num)
  })
  //println(pairFeats.toSeq.sortBy(- _._2(0)))
  
  /*************************************************************************************************/
  // Create factor graph
  import cc.factorie._
  import cc.factorie.la._
  import cc.factorie.variable._
  import cc.factorie.model._
  import scala.math._
  import cc.factorie.infer._
  // Create variables
  final val randomGenerator1 = new scala.util.Random
  final val randomGenerator2 = new scala.util.Random
  final val randomGenerator3 = new scala.util.Random
  def randomColor(): Color = {
    new Color((randomGenerator1.nextDouble,randomGenerator2.nextDouble,randomGenerator3.nextDouble))
  }
  class ColorVariable(v: Color) extends RefVariable[Color](v) 
  
  val colorVars =  Map[String, ColorVariable]()
  colorKeys.map(colorVars += _ -> new ColorVariable(randomColor()))
  
  colorVars += "Token" -> new ColorVariable(randomColor()) // background
  val colorSeqOfVars = colorVars.map{case (k,v) => v}(collection.breakOut) // without background
  
  
  // Create factors
  class PairwiseColorFactor(c1: ColorVariable, c2: ColorVariable, f: ((Double, Double, Double)) => Double) extends Factor2(c1, c2) {
    def score(v1: Color, v2: Color): Double = {
      val v1_Lab = v1.toLab()
      val v2_Lab = v2.toLab()
      
      val d = ((v1_Lab._1 - v2_Lab._1)/100.0, (v1_Lab._2 - v2_Lab._2)/128.0,  (v1_Lab._3 - v2_Lab._3)/128.0) 
      f(d)
    }
    override def factorName = "ColorFactor"
  }
  
  
  // Create model
  val m1 = new ItemizedModel  
  
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
  
  colorKeys.map({key =>
      val pKey = parent(key)
  	  //if (key != "Token" && pKey != "Token") {
  	  //  m1 ++= new PairwiseColorFactor(colorVars(key), colorVars(pKey), x => 0.0 )
  	  //}
      if (key != "Token") {
        m1 ++= new PairwiseColorFactor(colorVars(key), colorVars("Token"), x => log(CNDF((abs(x._1)-0.6)/0.03)+1E-6))
      }
  })
  
  
  println("origScore: " + m1.currentScore(colorSeqOfVars))
  
  // Create Sampler
  class MHColorSampler(model: Model) extends MHSampler[ColorVariable](model)(new scala.util.Random) {
    def propose(context:ColorVariable)(implicit d:DiffList) : Double = {
      val origScore = model.currentScore(context)
      context.set(randomColor())(d)
      val currScore = model.currentScore(context)
      currScore - origScore
    }
  }
  
  val sampler = new MHColorSampler(m1)
  sampler.processAll(colorSeqOfVars, 1000)
  println("modelScore: " + m1.currentScore(colorSeqOfVars))
  //println(colorVars.map(t => t._1 -> (m1.currentScore(t._2), t._2.value.toYIQ._1)))
  
  // Output color theme
  val jColorAST = colorVars.mapValues(_.value.toInt).toMap.toJson
  import java.io._
  val writeTheme = new PrintWriter(new File("../theme.json"))
  writeTheme.write(jColorAST.prettyPrint)
  writeTheme.close()
  
  
}
}