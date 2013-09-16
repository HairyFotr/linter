package com.foursquare.lint

sealed trait Warning {
  def message: String
  def name: String
}

object Warning {
  final val All: List[Warning] = 
    UnextendedSealedTrait ::
    UseLog1p ::
    UseExpm1 ::
    new UnlikelyEquality("left", "right") ::
    UseAbsNotSqrtPow ::
    UseAbsNotSqrtSquare ::
    UseIsNanNotSelfComparison ::
    UseIsNanNotNanComparison ::
    UseSigNum ::
    Nil 

  final val AllNames = All.map(_.name)

  final val NameToWarning: Map[String, Warning] = All.map(w => w.name -> w).toMap
}

sealed trait DefaultNameWarning extends Warning {
  def name = toString
}

sealed abstract class NoArgMessageWarning(val message: String) extends DefaultNameWarning

sealed abstract class OneArgMessageWarning(format: String, arg: String) extends Warning {
  def message = format.format(arg)
}
sealed abstract class TwoArgMessageWarning(format: String, arg1: String, arg2: String) extends Warning {
  def message = format.format(arg1, arg2)
}

case object UnextendedSealedTrait extends NoArgMessageWarning("This sealed trait is never extended")
case object UseLog1p extends NoArgMessageWarning("Use math.log1p(x) instead of math.log(1 + x) for added accuracy when x is near 0")
case object UseExpm1 extends NoArgMessageWarning("Use math.expm1(x) instead of math.exp(x) - 1 for added accuracy when x is near 0.")
class UnlikelyEquality(lhs: String, rhs: String) extends 
  TwoArgMessageWarning("Comparing with == on instances of different types (%s, %s) will probably return false.", lhs, rhs) {
  def name = "UnlikelyEquality"
}
case object UseAbsNotSqrtPow extends NoArgMessageWarning("Use abs instead of sqrt(pow(x, 2)).")
case object UseAbsNotSqrtSquare extends NoArgMessageWarning("Use abs instead of sqrt(x*x).")
case object UseIsNanNotSelfComparison extends NoArgMessageWarning("Use .isNan instead of comparing to itself.")
case object UseIsNanNotNanComparison extends NoArgMessageWarning("Use .isNan instead of comparing to NaN, which is wrong.")
case object UseSigNum extends NoArgMessageWarning("Did you mean to use the signum function here? (signum also avoids division by zero errors)")
case object BigDecimalNumberFormat extends NoArgMessageWarning("This BigDecimal constructor will likely throw a NumberFormatException.")
case object BigDecimalPrecisionLoss extends NoArgMessageWarning("Possible loss of precision - use a string constant")
case object ReflexiveAssignment extends NoArgMessageWarning("Assigning a variable to itself?")
case object CloseSourceFile extends NoArgMessageWarning("You should close the file stream after use.")
case object JavaConverters extends NoArgMessageWarning("Implicit conversions in collection.JavaConversions are dangerous. Consider using the explicit collection.JavaConverters")
class ContainsTypeMismatch(seqType: String, targetType: String) extends 
  TwoArgMessageWarning("%s.contains(%s) will probably return false because the collection and target element are different types.", seqType, targetType) {
  def name = "ContainsTypeMatch"
}
case object PatternMatchConstant extends NoArgMessageWarning("Pattern matching on a constant value.")
case object PreferIfToBooleanMatch extends NoArgMessageWarning("This is probably better written as an if statement.")
class IdenticalCaseBodies(n: String) extends OneArgMessageWarning("Bodies of %s neighbouring cases are identical and could be merged.", n) {
  def name = "IdenticalCaseBodies"
}
case object IdenticalCaseConditions extends NoArgMessageWarning("Identical case condition detected above. This case will never match.")
case object ReflexiveComparison extends NoArgMessageWarning("Same expression on both sides of comparison.")
case object YodaConditions extends NoArgMessageWarning("You are using Yoda conditions")
class UseConditionDirectly(negated: Boolean = false) extends OneArgMessageWarning("Remove the if and just use the %scondition.", if (negated) "negated " else "") {
  def name = "UseConditionDirectly"
}
case object DuplicateIfBranches extends NoArgMessageWarning("If statement branches have the same structure.")
case object IdenticalIfCondition extends NoArgMessageWarning("This condition has appeared earlier in the if-else chain and will never hold here.")
case object MergeNestedIfs extends NoArgMessageWarning("These two nested ifs can be merged into one.")
class VariableAssignedUnusedValue(variableName: String) extends OneArgMessageWarning("Variable %s has an unused value before this reassign.", variableName) {
  def name = "UseConditionDirectly"
}
