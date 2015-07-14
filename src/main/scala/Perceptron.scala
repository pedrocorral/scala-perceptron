
package pedrocorral {

  object Perceptron {

    type Doubles = List[Double]
    type Y_XS = Doubles
    type YS_XSS = List[Y_XS]
    type B_WS = Doubles

    val ε = 0.01

    def functionalTraining( ys_xss: Doubles* ): Option[Perceptron] = {
      lazy val n = ys_xss.head.length.toDouble

      def injectedTraining( ys_xss: List[Y_XS] ): Option[Perceptron] = {
        lazy val b_ws: B_WS = ys_xss.head.map( x => 0.0 )
        lazy val e0 = ys_xss.map( y_xs => error( y_xs, b_ws ) ).sum
        lazy val α = 1.0 / n

        def improveFromSample( y_xs: Y_XS, b_ws: B_WS ): (Double,B_WS) = {
          lazy val f_x = predictorFunction( y_xs.tail, b_ws.tail, b_ws.head )
          lazy val y = y_xs.head

          def improveRest( acc: Doubles, rest_xs: Doubles, rest_ws: Doubles ): Doubles =
            ((rest_xs, rest_ws): @unchecked) match {
              case (Nil,Nil) => acc.reverse
              case (x::xs, w::ws) => {
                val dw = ( y - f_x ) * x * α
                improveRest( (w+dw)::acc, xs, ws )
              }
            }

          val db = ( y - f_x ) / n
          val b = b_ws.head + db
          val ws = improveRest( Nil, y_xs.tail, b_ws.tail )
          ( error( y_xs, b_ws ), b :: ws )
        }

        def improve( depth: Int, e0: Double, e: Double, rest: List[Y_XS], b_ws: B_WS ): Option[Perceptron] = {
          rest match {
            case Nil if ( e < ε ) => Some( Perceptron( b_ws.tail, b_ws.head ) )
            case Nil if ( depth >= 100000 ) => None
            case Nil => improve( depth+1, e, 0.0, ys_xss, b_ws )
            case y_xs::ys_xss => improveFromSample( y_xs, b_ws ) match {
              case (e_y, b_ws: B_WS) => improve( depth, e0, e+e_y, ys_xss, b_ws )
            }
          }
        }

        improve( 0, e0, 0.0, ys_xss, b_ws )
      }

      training( injectedTraining, ys_xss.toList )
    }

    def optimizedTraining( xss: Doubles* ): Option[Perceptron] = {
      throw new Error( "To be done (but it is imperative, not functional) :P" )
      None
    }

    private def training( f: YS_XSS => Option[Perceptron], ys_xss: YS_XSS ): Option[Perceptron] =
      ys_xss match {
        case Nil => throw new Error( "Perpectron::apply::ERRoR::Empty training set" )
        case ys_xss => {
          lazy val n = ys_xss.head.length
          lazy val same_length = ys_xss.tail.forall( _.length == n )
          if ( !same_length ) throw new Error( "Perpectron::apply::ERRoR::The training set has different number of variables" )
          
          f( ys_xss )
        }
      }

    def σ( x: Double ) = 1.0 / ( 1.0 + Math.exp( -x ) )

    def predictorFunction( xs: Doubles, ws: Doubles, β: Double ): Double =
      σ( β + xs.zip(ws).map { case(x,w) => x * w }.sum )

    def error( y_xs: Y_XS, b_ws: B_WS ): Double =
      Math.abs( y_xs.head - predictorFunction( y_xs.tail, b_ws.tail, b_ws.head ) )

  }

  case class Perceptron( val ws: List[Double], val β: Double ) {

    def f( xs: List[Double] ) = Perceptron.predictorFunction( xs, ws, β )
    def bool( xs: List[Double] ) = f( xs ) > 0.5

    override def toString: String = ws.zipWithIndex.map { case(w,i) => s"w($i)=$w" }.mkString( "", ", ", "β=$β" )

  }


}
