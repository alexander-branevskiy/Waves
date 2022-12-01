package com.wavesplatform.lang.miniev

import com.wavesplatform.lang.miniev.Ev.Scope
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*

class LazyVal(var value: Either[(EXPR, Scope), EVALUATED]) {
  override def toString: String = value.fold(
    { case (expr, _) => s"N:$expr" },
    ev => s"E:$ev"
  )
}

trait Op {
  def ret(ev: EVALUATED): (EXPR, Option[Scope])
}

object Op {
  case class Get(field: String) extends Op {
    override def ret(ev: EVALUATED): (EXPR, Option[Scope]) = (ev match {
      case CaseObj(_, fields) =>
        fields.getOrElse[EXPR](field, FAIL(s"object $ev has no field $field"))
      case _ => FAIL(s"$ev is not a CaseObj")
    }) -> None
  }

  case class If(ifTrue: EXPR, ifFalse: EXPR) extends Op {
    override def ret(ev: EVALUATED): (EXPR, Option[Scope]) = (ev match {
      case CONST_BOOLEAN(cond) =>
        if (cond) ifTrue else ifFalse
      case _ => FAIL(s"$ev is not a Boolean")
    }) -> None
  }

  case class FuncArg(func: FunctionHeader, reversedEvaluatedArgs: List[EVALUATED], argsToEvaluate: List[EXPR]) extends Op {
    override def ret(ev: EVALUATED): (EXPR, Option[Scope]) =
      FUNCTION_CALL(func, (ev :: reversedEvaluatedArgs).foldLeft(argsToEvaluate) { case (a, v) => v :: a }) -> None
  }

  case class Value(key: String, lazyVal: LazyVal, state: State) extends Op {
    private val cachedScope = state.currentScope()

    override def ret(ev: EVALUATED): (EXPR, Option[Scope]) = {
      lazyVal.value = Right(ev)
      state.log(key, Right(ev))
      ev -> Some(cachedScope)
    }
  }

  case class Func(name: String, scope: Scope, predefinedComplexity: Option[Long] = None) extends Op {
    override def ret(ev: EVALUATED): (EXPR, Option[Scope]) = (ev, Some(scope))
  }
}
