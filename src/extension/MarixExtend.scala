package extension

import org.ujmp.core.Matrix
import org.ujmp.core.MatrixFactory

object MarixExtend {
	/**
	 * @author ExiaNan
	 */
	implicit class MatrixCalcuation(mat:Matrix) {

		def -(mat2:Matrix):Matrix = {
				return mat.minus(mat2)
		}

		def +(mat2:Matrix):Matrix = {
				return mat.plus(mat2)
		}

		def /(mat2:Matrix):Matrix = {
				return mat.divide(mat2)
		}
	}
}