
package pedrocorral {

  object Matrix {

    def apply( s: Any* ): Matrix = {
      Math.sqrt( s.length ).toInt match {
        case 0 => throw new Error( "Matrix::apply::ERRoR::Empty matrix" )
        case n if ( n*n == s.length ) => {

          def toDouble( x: Any ): Double = x match {
            case f: Float => f.toDouble 
            case d: Double => d
            case i: Int => i.toDouble
            case _ => throw new Error( s"Matrix::apply::ERRoR::Not a number => $x" )
          }
      
          def nSplit( ij: List[Array[Double]], acc: List[Double], j: Int, rest: List[Any] ): Matrix = rest match {
            case Nil => Matrix( ij.reverse.toArray )
            case x::xs if ( j < n ) => nSplit( ij, toDouble(x)::acc, j+1, xs )
            case x::xs if ( j == n ) => nSplit( (toDouble(x)::acc).toArray.reverse::ij, Nil, 1, xs )
          }

          nSplit( Nil, Nil, 1, s.toList )
        }
        case _ => throw new Error( "Matrix::apply::ERRoR::Not a squared matrix :S" )
      }
    }

  }

  case class Matrix( val ij: Array[Array[Double]] ) {

    if ( ij.length == 0 ) throw new Error( "Matrix::apply::ERRoR::Empty matrix" )
    else ij.foreach(
      x => if ( x.length != ij.head.length )
        throw new Error( "Matrix::apply::ERRoR::The matrix is not fixed size" )
    )

    def sizes: (Int,Int) = ij.length match {
      case 0 => (0,0)
      case n => (n,ij.head.length)
    }

    def determinant: Double = sizes match {
      case (2,2) => ij(0)(0)*ij(1)(1) - ij(0)(1)*ij(1)(0)
      case (n,m) if ( n == m ) => ij.head.zipWithIndex.map {
        case (e,x) => if ( x%2 == 0 )
          e * minor(1,x+1).determinant
        else
          -e * minor(1,x+1).determinant
      }.sum
      case _ => throw new Error( "Matrix::determinant::ERRoR::Not a squared matrix :S" )
    }

    def minor( i: Int, j: Int ): Matrix =
      sizes match {
        case (n,m) if ( i <= n && j <= m ) => Matrix(
          ij.zipWithIndex.collect {
            case (v,y) if ( y+1 != i ) => v.zipWithIndex.filter {
              case (_,x) => x+1 != j
            }.map( _._1 )
          }
        )
        case (n,m) => throw new IndexOutOfBoundsException( s"Matrix::minor::ERRoR::($i,$j) are out of ($n,$m)" )
      }

    def ==( M: Matrix ): Boolean =
      (sizes,M.sizes) match {
        case ((n0,m0),(n1,m1)) if ( n0==n1 && m0==m1 ) => ij.zip( M.ij ).forall {
          case (e0,e1) => e0 sameElements e1
        }
        case _ => false
      }

    override def toString: String = ij.map( _.mkString(" ") ).mkString("\n")

  }

}
