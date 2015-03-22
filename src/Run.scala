package preprocess

import scala.io.Source
import org.ujmp.core.MatrixFactory
import scala.collection.mutable.ArrayBuffer
import org.ujmp.core.enums.ValueType
import org.ujmp.core.Matrix
import Evaluation.ClassifierEvaluation
import org.ujmp.core.util.DistanceMeasure
import org.ujmp.core.util.EuclidianDistance
import Classifier.KNN
import scala.util.control.Breaks
import org.ujmp.core.calculation.Calculation.Ret

object Run {
  lazy val (irisMatrix, irisLabels) = {
    val source = Source.fromFile("iris.csv", "UTF-8")
    val lineIterator = source.getLines
    val doubleArray = ArrayBuffer[Seq[Double]]()
    val labels = ArrayBuffer[String]()

    val loop = new Breaks;

      for (l <- lineIterator if !l.isEmpty ) {
        val data = l.split(",")
        if (data.length == 0 || data==null)
          loop.break
        labels += data.last
        doubleArray.append(for (j <- (0 until data.length - 1)) yield data(j).toDouble)
      }

    val last = doubleArray.last
    val iris = MatrixFactory.dense(ValueType.DOUBLE, doubleArray.length, doubleArray.head.length)
    val length = doubleArray.length
    for (i <- 0 until doubleArray.length if doubleArray(i).length!=0; j <- 0 until doubleArray.head.length) {
      iris.setAsDouble(doubleArray(i)(j), i.toInt, j.toInt)
    }
    (iris, labels.toArray)
  }

  def main(args: Array[String]) {
    val mat = Tools.normMatrix(this.irisMatrix)
    val data = Tools.splitData(mat, this.irisLabels, 0.7)
    
    ClassifierEvaluation.evaluation(data._1, data._2, data._3, data._4, new KNN(5, 1, new EuclidianDistance()))  
  }
}