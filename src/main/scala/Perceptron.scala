
package pedrocorral {

  object Perceptron {

    def apply( xss: List[Any]* ): Option[Perceptron] =
      xss.toList match {
        case Nil => throw new Error( "Perpectron::apply::ERRoR::Empty training set" )
        case xss => {
          lazy val first_length = xss.head.length
          def checkLength( rest: List[Any] ): Boolean = rest match {
            case Nil => true
            case xs::xss => xs.length == first_length && checkLength( xss )
          }
          if ( !checkLength( xss.tail ) ) throw new Error( "Perpectron::apply::ERRoR::The training set has different number of variables" )
          
        }

  }

}
