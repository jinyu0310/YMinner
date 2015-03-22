package Classifier

import org.ujmp.core.Matrix



abstract class  Classifier {
	/**
	 * trainMat: training matrix,each row is a sample
	 * trainLabels：corresponding to the class of each row in trainMat
	 */
	def train(trainMat:Matrix,trainLabels:Array[String])

	/**
	 * instances:each row is a sample
	 * */
	def classInstances(instances:Matrix):Array[String]
}