package Classifier

import org.ujmp.core.Matrix
import org.ujmp.core.util.DistanceMeasure
import preprocess.Tools
import org.ujmp.core.calculation.Calculation.Ret
import YCategory.ArrayExtend._

/***
 * k: the k number nearest data points
 * thread: if not set to 1, the classifier will work in a multiple thread mode.
 */
class KNN(k:Int, thread:Int,measure:DistanceMeasure) extends Classifier {
  
  private var trainMat : Matrix = null
  private var trainLabels : Array[String] = null
  private var distance:DistanceMeasure = measure
  
  def classInstances(dataset: Matrix): Array[String] = {
      val instances = dataset
      val distances = Tools.allDistance(instances, this.trainMat, distance)
    
      val result = new Array[String]((instances.getRowCount()).toInt)
      
      for (i <- 0l until instances.getRowCount()){
          var sortRows = distances.selectRows(Ret.LINK, i)
          var sorts = sortRows.toDoubleArray()(0)
          var zipsort = sorts.zipWithIndex.sortBy(_._1)
          val sortIndex = sortRows.toDoubleArray()(0).argSort
          import scala.collection.mutable.Map  
          val count = Map[String,Int]()
          for(index <- sortIndex.slice(0,k))
          {
            if(! count.contains( trainLabels(index) )){
              count( trainLabels(index) ) = 0
            }else{
              count(trainLabels(index)) += 1
            }
          }
          result(i.toInt) = count.max._1
      }
      
      var set = result.toSet
      return result
  }

  def train(trainMat: Matrix, trainLabels: Array[String]): Unit = {
      this.trainMat = trainMat
      this.trainLabels = trainLabels
      return 
  }
}

