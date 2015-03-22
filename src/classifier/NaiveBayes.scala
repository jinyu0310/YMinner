package classifier

import org.ujmp.core.Matrix
import org.ujmp.core.calculation.Calculation.Ret
import distribution.Gaussian
import scala.collection.mutable._

class NaiveBayes(discrete: Boolean) extends Classifier {

    
  private val enableGaussian = !discrete

  private lazy val clsProbs = {
    val clsCount = Map[String, Int]()
    
    for (cls <- trainLabels) {
      if(!clsCount.contains(cls))
        clsCount(cls) = 0
      clsCount(cls) = clsCount(cls) + 1
    }
    
    val cp = Map[String, Double]()
    
    for ((cls, count) <- clsCount) {
      cp(cls) = count * 1.0 / trainLabels.length
    }
    
    cp
  }

  //mu Map(label - > probVector)
  private lazy val (mu, sigma) = {
    val indecies = Map[String, ArrayBuffer[Long]]()
    val mus = Map[String, Array[Double]]()
    val sigmas = Map[String, Array[Double]]()

    for (i <- 0 until this.trainLabels.length) {
      val label = this.trainLabels(i)
      if (!indecies.contains(label)) {
        indecies(label) = ArrayBuffer[Long](i.toLong)
      } else {
        val arr = indecies(label)
        arr += i.toLong
      }
    }

    for ((label, indexCls) <- indecies) {
      val mat = this.trainMat.selectRows(Ret.LINK, indexCls: _*)
      val sigma = mat.`var`(Ret.LINK, 0, true)
      val mu = mat.mean(Ret.LINK, 0, true)
      mus(label) = mu.toDoubleArray().head
      sigmas(label) = sigma.sqrt(Ret.LINK).toDoubleArray().head
    }

    (mus, sigmas)
  }

  //classLabel -> (key -> ratio)
  //in which key = is a string as "featureNO_value": 1_1
  private lazy val probItemInCls: Map[String, Map[String, Double]] = {
    val map = Map[String, Map[String, Double]]()
    val classCounts = Map[String,Int]()
    for (i <- 0l until this.trainMat.getRowCount) {
     val label = this.trainLabels(i.toInt)
     if(!classCounts.contains(label)){
       classCounts(label) = 0
     }
      classCounts(label) += 1

      for (j <- 0l until this.trainMat.getColumnCount) {
        val clsProbs = map(label) //Map[Int,Int] item->count
        val value = this.trainMat.getAsInt(i, j)
        if (!clsProbs.contains(j+"_"+value)) {
          clsProbs(j+"_"+value) = 0
        }
        clsProbs(j+"_"+value) += 1
      }
    }

    for ((clsLabel, clsCount) <- map) {
      for((item,count) <- clsCount){
        clsCount(item) = count / classCounts(clsLabel)
      }
    }
    
    map
  }

  def classInstances(instances: Matrix): Array[String] = {
     val instances = this.trainMat
     val result = new Array[String]((instances.getRowCount()).toInt)
     for(i <- 0l until instances.getRowCount){
       val vector = instances.selectRows(Ret.LINK, i).toDoubleArray().head
       var s = (for((cls,_) <- this.clsProbs) yield (cls,this.probOf(vector, cls)))
       result(i.toInt) = (for((cls,_) <- this.clsProbs) yield (cls,this.probOf(vector, cls))).maxBy(_._2)._1
     }
     println(result.toSet)
     return result
  }

  def train(trainMat: Matrix, trainLabels: Array[String]): Unit = {
    this.trainMat = trainMat
    this.trainLabels = trainLabels
    return
  }

  
  
  /***
   * @description: return the probability of a value in a given feature of a class
   * 
   */
  private def probOfValue(value:Double,feature:Int,cate:String):Double = {
        if(this.enableGaussian){
          val mu = this.mu(cate)(feature)
          val sigma = this.sigma(cate)(feature)
          return Gaussian.probability(sigma, mu, value)
        }else{
          val clsProb = this.probItemInCls(cate)
          return clsProb(feature + "_" + value)
        }
  }
  
  private def probOf(vector: Array[Double], cate: String): Double = {
    var result = 1.0
    for (feature <- 0 until vector.length) {
      val value = vector(feature)
      val itemClsProb = this.probOfValue(value,feature,cate)
      result *= this.bayesProbability(this.clsProbs(cate), itemClsProb)
    }
    return result
  }

  def bayesProbability(classProb: Double, itemClsProb: Double): Double = {
    return (itemClsProb * classProb) //
  }

}