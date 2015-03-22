package distribution

object Gaussian {
    def probability(sigma:Double,mu:Double,x:Double):Double = {
     return (1/(Math.sqrt(2*Math.PI)*sigma) * Math.exp(-(x-mu)*(x-mu) / (2*sigma*sigma)))
    }       
}