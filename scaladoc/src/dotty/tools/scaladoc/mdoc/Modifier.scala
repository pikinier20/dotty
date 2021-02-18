package dotty.tools.scaladoc.mdoc

import dotty.tools.scaladoc.mdoc.Mod._

sealed abstract class Modifier(val mods: Set[Mod]) {
  def isDefault: Boolean = mods.isEmpty
  def isFailOrWarn: Boolean = isFail || isWarn
  def isFail: Boolean = mods(Fail)
  def isWarn: Boolean = mods(Warn)
  def isPassthrough: Boolean = mods(Passthrough)
  def isCrash: Boolean = mods(Crash)
  def isSilent: Boolean = mods(Silent)
  def isInvisible: Boolean = mods(Invisible)
  def isCompileOnly: Boolean = mods(CompileOnly)
  def isReset: Boolean = isResetClass || isResetObject
  def isResetClass: Boolean = mods(ResetClass) || mods(Reset)
  def isResetObject: Boolean = mods(ResetObject)
  def isNest: Boolean = mods(Nest)

  def widthOverride: Option[Int] =
    mods.collectFirst { case Width(value) =>
      value
    }

  def heightOverride: Option[Int] =
    mods.collectFirst { case Height(value) =>
      value
    }

  def isToString: Boolean = mods(ToString)
}
object Modifier {
  object Default {
    def apply(): Modifier = Builtin(Set.empty)
  }
  object Crash {
    def unapply(m: Modifier): Boolean =
      m.isCrash
  }
  object Fail {
    def unapply(m: Modifier): Boolean =
      m.isFailOrWarn
  }
  object Warn {
    def unapply(m: Modifier): Boolean =
      m.isWarn
  }
  object PrintVariable {
    def unapply(m: Modifier): Boolean =
      m.isDefault || m.isPassthrough || m.isReset
  }

  def apply(string: String): Option[Modifier] = {
    val mods = string.split(":").map {
      case Mod(m) => Some(m)
      case _ => None
    }
    if (mods.forall(_.isDefined)) {
      Some(Builtin(mods.iterator.map(_.get).toSet))
    } else {
      None
    }
  }

  case class Builtin(override val mods: Set[Mod]) extends Modifier(mods)

}
