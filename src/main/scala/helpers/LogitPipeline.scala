
package pedrocorral.helpers {

  object LogicPipeline {

    implicit class PipelineContainer[A]( val a: A ) extends AnyVal {

      def -->[B]( f: A => B ): B = f(a)
      def -+>[B]( f: A => B ): A = { f(a) ; a }
      def ++>[B]( b: B ): A = a

    }

  }

}
