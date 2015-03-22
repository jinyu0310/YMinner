package classifier

import org.ujmp.core.Matrix




abstract class  Classifier {

  protected var trainMat : Matrix = null
  protected var trainLabels : Array[String] = null
  
  def getTrainMat = trainMat
	def getTrainLabels = trainLabels

  /**
	 * trainMat: training matrix,each row is a sample(make sure all of the entries i numeric)
	 * trainLabels：corresponding to the class of each row in trainMat
	 */
	def train(trainMat:Matrix,trainLabels:Array[String])

	/**
	 * instances:each row is a sample
	 * */
	def classInstances(instances:Matrix):Array[String]
}