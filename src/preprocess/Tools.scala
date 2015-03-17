package preprocess

import org.ujmp.core.Matrix
import org.ujmp.core.calculation.Calculation.Ret
import org.ujmp.core.MatrixFactory
import org.ujmp.core.enums.ValueType
import org.ujmp.core.objectmatrix.calculation.Repmat
import org.ujmp.core.interfaces.DistanceMeasures
import org.ujmp.core.util.EuclidianDistance
import org.ujmp.core.util.EuclidianDistance
import org.ujmp.core.util.DistanceMeasure
import YCategory.MarixExtend._
import scala.collection.mutable.ArrayBuffer
import scala.actors.threadpool.helpers.Utils

object Tools {
  def normMatrix(mat: Matrix): Matrix = {
    val minVector: Matrix = mat.min(Ret.LINK, 1)
    val maxVector: Matrix = mat.max(Ret.LINK, 1)
    val minMat = repmat(minVector, 1, mat.getSize(1))
    val maxMat = repmat(maxVector, 1, mat.getSize(1))
    val normMat = (mat - minMat) / (maxMat - minMat)
    return normMat
  }

  /**
   * repeat a matrix
   * */
  def repmat(mat: Matrix, r: Long, c: Long): Matrix = {
    val repmat: Repmat = new Repmat(mat, r, c)
    return repmat.calcNew()
  }
  
  /**
   * @Description:
   *  Randomly split original matrix into train set and test set
   * 
   * @Parameter:
   *  Ratio, size of trainset
   * 
   * @Return:
   *  (trainMatrix,trainLabels,testMatrix,testLabels)
   */
  def splitData(mat:Matrix,labels:Array[String],ratio:Double):(Matrix,Array[String],Matrix,Array[String]) = {
    var trainIndecies = ArrayBuffer[Long]()
    var testIndecies  = ArrayBuffer[Long]()
    var trainLabels = ArrayBuffer[String]()
    var testLabels = ArrayBuffer[String]()
    
    for(i <-0 until mat.getRowCount.toInt){
      if(scala.util.Random.nextDouble()<=ratio){
        trainIndecies += i
        trainLabels += labels(i)
      }else{
        testIndecies += i
        testLabels += labels(i)
      }
    }
    
    var trainMat = mat.selectRows(Ret.NEW, trainIndecies:_*)
    var testMat =  mat.selectRows(Ret.NEW, testIndecies:_*)
    
    return (trainMat,trainLabels.toArray,testMat,testLabels.toArray)
  }

  /**
   * generate a distance matrix,
   * each entry (i,j) means the distance between ith row
   * in mat1 and jth row in mat2
   */
  def allDistance(mat1: Matrix, mat2: Matrix, distance: DistanceMeasure): Matrix = {    
    val dm = MatrixFactory.dense(ValueType.DOUBLE, mat1.getRowCount(), mat2.getRowCount())
    val check = MatrixFactory.dense(ValueType.DOUBLE, mat1.getRowCount(), mat2.getRowCount())
    for(i <- 0l until dm.getRowCount() ; j <- 0l until dm.getRowCount()){
        val sample1 = mat1.selectRows(Ret.LINK, i).toDoubleArray()(0)
        val sample2 = mat2.selectRows(Ret.LINK, j).toDoubleArray()(0)
        val value = distance.getDistance(sample1, sample2)
        dm.setAsDouble(value,i,j)
    }
    return dm
  }

    def main(args:Array[String]){
       var matrix = MatrixFactory.randn(ValueType.DOUBLE, 5,2);
       println(matrix);
       var matrix2 = MatrixFactory.randn(ValueType.DOUBLE, 5,2);
       println(matrix2)
       val eul = new EuclidianDistance()
       println(allDistance(matrix, matrix2,eul))
    }
    
}