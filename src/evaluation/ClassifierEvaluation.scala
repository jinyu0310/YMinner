package evaluation

import org.ujmp.core.Matrix
import classifier.Classifier
import preprocess.Tools

object ClassifierEvaluation {
    def evaluation(train:Matrix,trainLabel:Array[String],test:Matrix,testLabel:Array[String],classifier:Classifier):Unit = {
        val normTrainMat =  Tools.normMatrix(train)
        val normTestMat  =  Tools.normMatrixWithTrain(test, train)
        classifier.train(normTrainMat, trainLabel)
        var predicate =  classifier.classInstances(test)
        var correct = 0
        for( i <- 0 until testLabel.length){
          if( predicate(i) == testLabel(i) ){
            correct += 1
          }
        }
        println(s"classifier:   ${classifier.getClass.getName}")
        println(s"Correct:$correct Total:${testLabel.length} Ratio:${correct*1.0 / testLabel.length}")
    }
}