package com.wavesplatform.lang.v1.evaluator.ctx

import java.util

import cats.*
import cats.syntax.functor.*
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.LET
import com.wavesplatform.lang.v1.compiler.Types.FINAL
import com.wavesplatform.lang.v1.evaluator.{LetExecResult, LetLogCallback}
import com.wavesplatform.lang.v1.traits.Environment
import shapeless.{Lens, lens}

case class EvaluationContext[F[_]](
    environment: Environment[F],
    typeDefs: Map[String, FINAL],
    letDefs: Map[String, LazyVal[F]],
    functions: Map[FunctionHeader, BaseFunction]
) {
  def mapK[G[_]: Monad](f: F ~> G): EvaluationContext[G] =
    EvaluationContext(
      environment.asInstanceOf[Environment[G]],
      typeDefs,
      letDefs.view.mapValues(_.mapK(f)).toMap,
      functions
    )
}

case class LoggedEvaluationContext[F[_]: Monad](l: LetLogCallback[F], ec: EvaluationContext[F]) {
  val loggedLets: util.IdentityHashMap[LET, Unit]          = new util.IdentityHashMap()
  val loggedErrors: collection.mutable.Set[ExecutionError] = collection.mutable.Set()

  def log(let: LET, result: LetExecResult[F]): F[Unit] = {
    result.map {
      case Left(err) if !loggedErrors.contains(err) =>
        loggedErrors.addOne(err)
        add(let, result)
      case Left(_) => ()
      case _       => add(let, result)
    }
  }

  private def add(let: LET, result: LetExecResult[F]): Unit = {
//    loggedLets.computeIfAbsent(let, _ => l(let.name)(result))
  }
}

object LoggedEvaluationContext {
  class Lenses[F[_]: Monad] {
    val types: Lens[LoggedEvaluationContext[F], Map[String, FINAL]]     = lens[LoggedEvaluationContext[F]] >> Symbol("ec") >> Symbol("typeDefs")
    val lets: Lens[LoggedEvaluationContext[F], Map[String, LazyVal[F]]] = lens[LoggedEvaluationContext[F]] >> Symbol("ec") >> Symbol("letDefs")
    val funcs: Lens[LoggedEvaluationContext[F], Map[FunctionHeader, BaseFunction]] =
      lens[LoggedEvaluationContext[F]] >> Symbol("ec") >> Symbol("functions")
  }
}

object EvaluationContext {
  def build[F[_]](
      environment: Environment[F],
      typeDefs: Map[String, FINAL],
      letDefs: Map[String, LazyVal[F]],
      functions: Seq[BaseFunction]
  ): EvaluationContext[F] = {
    if (functions.distinct.size != functions.size) {
      val dups = functions.groupBy(_.header).filter(_._2.size != 1)
      throw new Exception(s"Duplicate runtime functions names: $dups")
    }
    EvaluationContext(environment, typeDefs, letDefs, functions.map(f => f.header -> f).toMap)
  }
}
