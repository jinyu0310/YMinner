package YCategory
import scala.math._
object ArrayExtend {
	implicit class ArrayExtensionInt(val array: Array[Int]) {
		def argSort(): Array[Int] = {
				var tupleArray = for (i <- 0 until array.length) yield (i, array(i))     

						tupleArray = tupleArray.sortWith((x,y)=> x._2 < y._2)

						val result = for((index,_) <- tupleArray) yield index

						return result.toArray
		}
	}


	implicit class ArrayExtensionDouble(val array: Array[Double]) {
		def argSort(): Array[Int] = {
				var tupleArray = for (i <- 0 until array.length) yield (i, array(i))     

						tupleArray = tupleArray.sortWith((x,y)=> x._2 < y._2)

						val result = for((index,_) <- tupleArray) yield index

						return result.toArray
		}
	}

	implicit class ArrayExtensionLong(val array: Array[Long]) {
		def argSort(): Array[Int] = {
				var tupleArray = for (i <- 0 until array.length) yield (i, array(i))     

						tupleArray = tupleArray.sortWith((x,y)=> x._2 < y._2)

						val result = for((index,_) <- tupleArray) yield index

						return result.toArray
		}
	}


//	def main(args:Array[String]){
//		val array = Array(5,4,3,2,0)
//				for(i <- array.argSort()) print(i)
//	}

}