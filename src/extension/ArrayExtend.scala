package YCategory
import scala.math._
object ArrayExtend {

	implicit class ArrayExtension[T <% Ordered[T]](val array: Array[T]) {
		def argSort = array.zipWithIndex.sortBy(_._1).map(_._2).toArray
	}

	def main(args:Array[String]){
		val array = Array(5,4,3,2,0)
				for(i <- array.argSort) print(i)
	}

}