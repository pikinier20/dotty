package dotty.tools
package dotc
package core

import Types._
import Flags._
import Contexts._
import util.{SimpleMap, DotClass}
import reporting._
import printing.{Showable, Printer}
import printing.Texts._
import collection.mutable

class TyperState(r: Reporter) extends DotClass with Showable {

  /** The current reporter */
  def reporter = r

  /** The current constraint set */
  def constraint: Constraint = new Constraint(SimpleMap.Empty, SimpleMap.Empty)
  def constraint_=(c: Constraint): Unit = {}

  /** The uninstantiated variables */
  def uninstVars = constraint.uninstVars

  /** Gives for each instantiated type var that does not yet have its `inst` field
   *  set, the instance value stored in the constraint. Storing instances in constraints
   *  is done only in a temporary way for contexts that may be retracted
   *  without also retracting the type var as a whole.
   */
  def instType(tvar: TypeVar): Type = constraint.at(tvar.origin) match {
    case _: TypeBounds => NoType
    case tp: PolyParam =>
      var tvar1 = constraint.typeVarOfParam(tp)
      if (tvar1.exists) tvar1 else tp
    case tp => tp
  }

  /** A fresh typer state with the same constraint as this one.
   *  @param isCommittable  The constraint can be committed to an enclosing context.
   */
  def fresh(isCommittable: Boolean): TyperState = this

  /** A fresh type state with the same constraint as this one and the given reporter */
  def withReporter(reporter: Reporter) = new TyperState(reporter)

  /** Commit state so that it gets propagated to enclosing context */
  def commit()(implicit ctx: Context): Unit = unsupported("commit")

  /** Make type variable instances permanent by assigning to `inst` field if
   *  type variable instantiation cannot be retracted anymore. Then, remove
   *  no-longer needed constraint entries.
   */
  def gc()(implicit ctx: Context): Unit = ()

  /** Is it allowed to commit this state? */
  def isCommittable: Boolean = false

  /** Can this state be transitively committed until the top-level? */
  def isGlobalCommittable: Boolean = false

  def tryWithFallback[T](op: => T)(fallback: => T)(implicit ctx: Context): T = unsupported("tryWithFallBack")

  override def toText(printer: Printer): Text = "ImmutableTyperState"
}

class MutableTyperState(previous: TyperState, r: Reporter, override val isCommittable: Boolean)
extends TyperState(r) {

  private var myReporter = r

  override def reporter = myReporter

  private var myConstraint: Constraint = previous.constraint

  override def constraint = myConstraint
  override def constraint_=(c: Constraint) = myConstraint = c

  override def fresh(isCommittable: Boolean): TyperState =
    new MutableTyperState(this, new StoreReporter, isCommittable)

  override def withReporter(reporter: Reporter) =
    new MutableTyperState(this, reporter, isCommittable)

  override val isGlobalCommittable =
    isCommittable &&
    (!previous.isInstanceOf[MutableTyperState] || previous.isGlobalCommittable)

  /** Commit typer state so that its information is copied into current typer state
   *  In addition (1) the owning state of undetermined or temporarily instantiated
   *  type variables changes from this typer state to the current one. (2) Variables
   *  that were temporarily instantiated in the current typer state are permanently
   *  instantiated instead.
   */
  override def commit()(implicit ctx: Context) = {
    val targetState = ctx.typerState
    assert(isCommittable)
    targetState.constraint = constraint

    constraint foreachTypeVar { tvar =>
      if (tvar.owningState eq this)
        tvar.owningState = targetState
    }
    targetState.gc()
    reporter.flush()
  }

  override def gc()(implicit ctx: Context): Unit = {
    val toCollect = new mutable.ListBuffer[PolyType]
    constraint foreachTypeVar { tvar =>
      if (!tvar.inst.exists) {
        val inst = instType(tvar)
        if (inst.exists && (tvar.owningState eq this)) {
          tvar.inst = inst
          val poly = tvar.origin.binder
          if (constraint.isRemovable(poly)) toCollect += poly
        }
      }
    }
    for (poly <- toCollect)
      constraint = constraint.remove(poly)
  }

  /** Try operation `op`; if it produces errors, execute `fallback` with constraint and
   *  reporter as they were before `op` was executed. This is similar to `typer/tryEither`,
   *  but with one important difference: Any type variable instantiations produced by `op`
   *  are persisted even if `op` fails. This is normally not what one wants and therefore
   *  it is recommended to use
   *
   *      tryEither { implicit ctx => op } { (_, _) => fallBack }
   *
   *  instead of
   *
   *      ctx.tryWithFallback(op)(fallBack)
   *
   *  `tryWithFallback` is only used when an implicit parameter search fails
   *  and the whole expression is subsequently retype-checked with a Wildcard
   *  expected type (so as to allow an implicit conversion on the result and
   *  avoid over-constraining the implicit parameter search). In this case,
   *  the only type variables that might be falsely instantiated by `op` but
   *  not by `fallBack` are type variables in the typed expression itself, and
   *  these will be thrown away and new ones will be created on re-typing.
   *  So `tryWithFallback` is safe. It is also necessary because without it
   *  we do not propagate enough instantiation information into the implicit search
   *  and this might lead to a missing parameter type error. This is exhibited
   *  at several places in the test suite (for instance in `pos_typers`).
   *  Overall, this is rather ugly, but despite trying for 2 days I have not
   *  found a better solution.
   */
  override def tryWithFallback[T](op: => T)(fallback: => T)(implicit ctx: Context): T = {
    val savedReporter = myReporter
    val savedConstraint = myConstraint
    myReporter = new StoreReporter
    val result = op
    try
      if (!reporter.hasErrors) result
      else {
        myConstraint = savedConstraint
        fallback
      }
    finally myReporter = savedReporter
  }

  override def toText(printer: Printer): Text = constraint.toText(printer)
}
