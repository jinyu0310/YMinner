package Classifier

import org.ujmp.core.Matrix

object ClassifierEvaluation {
    def evaluation(train:Matrix,trainLabel:Array[Long],test:Matrix,testLabel:Array[Long],classifier:Classifier):Unit = {
        classifier.train(train, trainLabel)
        var predicate =  classifier.classInstances(test)
        var correct = 0
        for( i <- 0 until testLabel.length){
          if( predicate(i) == testLabel(i) ){
            correct += 1
          }
        }
        println(s"Correct:$correct Total:${testLabel.length} Ratio:${correct*1.0 / testLabel.length}")
    }
}