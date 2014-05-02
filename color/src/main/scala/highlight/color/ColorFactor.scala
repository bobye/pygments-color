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
  
  val tokenFeatsBuf = Map[String, List[Double]]()
  tokenKeys.map(key => tokenFeatsBuf += key -> {
    val raw = unaryMap(key)
    raw.convertTo[List[Double]]
  })  
  //println(tokenFeatsBuf.toSeq.sortBy( - _._2(0)))
  val totalNumOfChars = tokenFeatsBuf.map{case (k,v) => v(0)}(collection.breakOut).sum
  val totalNumOfLines = tokenFeatsBuf("Token")(1)
  tokenFeatsBuf -= "Token" 
  //println(totalNumOfChars, totalNumOfLines)
  val tokenFeats = tokenFeatsBuf.mapValues(l =>
    List(l(0)/totalNumOfChars, l(1)/totalNumOfLines, l(2), l(3), l(4)))
    
  val defaultTokenSet = tokenFeats.filter(_._2(0) > 0.1).keys.toSet
  println(defaultTokenSet)
  println(tokenFeats.filter(_._2(1) > 0.1).keys)
  
  val pairFeatsBuf = Map[(String, String), List[Double]]()
  pairKeys.map(key => pairFeatsBuf += {
    val tkey = key.drop(1).dropRight(1).split(", ") match {case Array(a,b,_*) => (a,b)} 
    tkey -> {
    val raw = pairMap(key)
    val l = raw.convertTo[List[(Int, Int)]]
    val num = l.length
    val avg = l.reduce((v1,v2) => (v1._1 + v2._1, v1._2 + v2._2))
    List(num, avg._1.toDouble/num, avg._2.toDouble/num)
  }})
  //println(pairFeatsBuf.toSeq.sortBy(- _._2(0)))
  val totalNumOfPairs = pairFeatsBuf.map{case (k,v) => v(0)}(collection.breakOut).sum
  val pairFeats = pairFeatsBuf.mapValues(l =>
    List(l(0)/totalNumOfPairs, l(1), l(2)))
  println(pairFeats.filter( _._2(0) > 0.05).keys)
  
  /*************************************************************************************************/
  // Create factor graph
  import cc.factorie._
  import cc.factorie.la._
  import cc.factorie.variable._
  import cc.factorie.model._

  import cc.factorie.infer._
  // Create variables

  def randomColor(): Color = {
    val randomGenerator = new scala.util.Random
    new Color((randomGenerator.nextDouble,randomGenerator.nextDouble,randomGenerator.nextDouble))
  }
  /*
  def randomColor(c: Color, mu: Double): Color = {
    val randomGenerator = new scala.util.Random
    val r = c.r + mu * (randomGenerator.nextDouble - 0.5)
    val g = c.g + mu * (randomGenerator.nextDouble - 0.5)
    val b = c.b + mu * (randomGenerator.nextDouble - 0.5)
    def f(x: Double): Double = {
      if (x>1) 1 
      else if (x<0) 0 
      else x
    }
    new Color((f(r), f(g), f(b)))
  }
*/
  class ColorVariable(v: Color) extends RefVariable[Color](v) 
  
  val colorVars =  Map[String, ColorVariable]()
  colorKeys.map(colorVars += _ -> new ColorVariable(randomColor()))
  
  colorVars += "Token" -> new ColorVariable(randomColor()) // background
  val colorSeqOfVars = colorVars.map{case (k,v) => v}(collection.breakOut) 
  
  
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
  
  import ColorProperty._
  import Function._
  // Create model
  val m1 = new ItemizedModel  
  import scala.math._
  
  
  defaultTokenSet.map(key => {
     m1 ++= new UnaryColorFactor(colorVars(key),
         saturation,
         x => -x/0.02)
  })
  
  pairFeats.filter(_._2(0) > 0.05).keys.map(key => {
     m1 ++= new PairwiseColorFactor(colorVars(key._1), colorVars(key._2),
         chromaticDifference,
         x => -x/0.1)
     m1 ++= new PairwiseColorFactor(colorVars(key._1), colorVars(key._2),
         relativeLightness,
         x => -x/0.1)
  })
         
  // pairwise
  colorKeys.map({key =>
      val pKey = parent(key)
  	  //if (key != "Token" && pKey != "Token") {
  	  //  m1 ++= new PairwiseColorFactor(colorVars(key), colorVars(pKey), x => 0.0 )
  	  //}
      if (key != "Token") {
        var cutoff = 0.5
        if (defaultTokenSet contains key) cutoff = 0.7
        
        m1 ++= new PairwiseColorFactor(colorVars(key), colorVars("Token"), 
            relativeLightness, 
            x => log(CNDF((abs(x)-cutoff)/0.03)+1E-10))
        
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
  sampler.processAll(colorSeqOfVars, 10000)
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