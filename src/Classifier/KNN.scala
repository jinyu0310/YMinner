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
  private var trainLabels : Array[Long] = null
  private var distance:DistanceMeasure = measure
  
  def classInstances(instances: Matrix): Array[Long] = {
    
      val distances = Tools.allDistance(instances, this.trainMat, distance)
    
      val result = Array[Long]((instances.getRowCount()-1).toInt)
      
      for (i <- 0l until instances.getRowCount()){
          var sortRows = distances.selectRows(Ret.LINK, i)
          val sortIndex = sortRows.toDoubleArray()(0).argSort()
          import scala.collection.mutable.Map  
          val count = Map[Long,Int]()
          for(index <- sortIndex)
          {
            if(! count.contains( trainLabels(index) )){
              count( trainLabels(index) ) = 0
            }else{
              count(trainLabels(index)) +=1
            }
          }
          result(i.toInt) = count.max._2.toLong
      }
      
      return result
  }

  def train(trainMat: Matrix, trainLabels: Array[Long]): Unit = {
      this.trainMat = trainMat
      this.trainLabels = trainLabels
      return 
  }


}
