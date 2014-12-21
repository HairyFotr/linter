/**
 *   Copyright 2012 Foursquare Labs, Inc.
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

package com.foursquare.lint

sealed trait Warning {
  def message: String
  def name: String
}
// scalastyle:off public.methods.have.type
object Warning {
  final val All: Seq[Warning] = Vector(
    AssigningOptionToNull,
    AvoidOptionCollectionSize,
    new AvoidOptionMethod("", ""),
    AvoidOptionStringSize,
    BigDecimalNumberFormat,
    BigDecimalPrecisionLoss,
    CloseSourceFile,
    new ContainsTypeMismatch("", ""),
    new NumberInstanceOf(""),
    new DecomposingEmptyCollection("", ""),
    ModuloByOne,
    DivideByOne,
    DivideByZero,
    ZeroDivideBy,
    DuplicateIfBranches,
    DuplicateKeyInMap,
    TransformNotMap,
    new IdenticalCaseBodies(""),
    IdenticalCaseConditions,
    IdenticalIfCondition,
    IdenticalIfElseCondition,
    IdenticalStatements,
    IndexingWithNegativeNumber,
    new InefficientUseOfListSize(""),
    IntDivisionAssignedToFloat,
    InvalidParamToRandomNextInt,
    new InvalidStringConversion(""),
    new InvalidStringFormat(""),
    new InvariantCondition(always = true, "hold"),
    new InvariantExtrema(max = true, returnsFirst = true),
    new InvariantReturn("method", "returnValue"),
    JavaConverters,
    new LikelyIndexOutOfBounds(""),
    MalformedSwap,
    MergeNestedIfs,
    OnceEvaluatedStatementsInBlockReturningFunction,
    new OperationAlwaysProducesZero(""),
    OptionOfOption,
    new PassPartialFunctionDirectly(""),
    new UnitImplicitOrdering(""),
    PatternMatchConstant,
    new PossibleLossOfPrecision(""),
    PreferIfToBooleanMatch,
    ProducesEmptyCollection,
    ReflexiveAssignment,
    ReflexiveComparison,
    new RegexWarning("", false),
    StringMultiplicationByNonPositive,
    new UndesirableTypeInference(""),
    UnextendedSealedTrait,
    new UnlikelyEquality("lhs", "rhs"),
    new UnnecessaryMethodCall(""),
    UnnecessaryReturn,
    UnnecessaryStringIsEmpty,
    UnnecessaryStringNonEmpty,
    UnusedForLoopIteratorValue,
    new UnusedParameter(Seq("unusedParameter"), "methodName"),
    UseHypot,
    UseLog10,
    UseCbrt,
    UseSqrt,
    UseExp,
    UseAbsNotSqrtSquare,
    new UseConditionDirectly(negated = false),
    new UseIfExpression(""),
    new UseExistsOnOption("", ""),
    UseExpm1,
    UseFilterNotFlatMap,
    UseFlattenNotFilterOption,
    UseGetOrElseOnOption,
    UseFindNotFilterHead,
    UseIsNanNotNanComparison,
    UseIsNanNotSelfComparison,
    UseLog1p,
    new UseOptionGetOrElse(""),
    new UseOptionOrNull(""),
    UseSignum,
    UseUntilNotToMinusOne,
    new VariableAssignedUnusedValue(""),
    WrapNullWithOption,
    YodaConditions,
    new UnsafeAbs(""),
    new TypeToType(""),
    EmptyStringInterpolator,
    new UnlikelyToString(""),
    UnthrownException,
    SuspiciousMatches,
    IfDoWhile)

  final val AllNames = All.map(_.name)

  final val NameToWarning: Map[String, Warning] = All.map(w => w.name -> w).toMap
}

sealed trait DefaultNameWarning extends Warning {
  def name = toString
}

sealed abstract class NoArgWarning(val message: String) extends DefaultNameWarning

sealed abstract class OneArgWarning(format: String, arg: String) extends Warning {
  def message = format.format(arg)
}
sealed abstract class TwoArgWarning(format: String, arg1: String, arg2: String) extends Warning {
  def message = format.format(arg1, arg2)
}

case object UnextendedSealedTrait extends NoArgWarning("This sealed trait is never extended")
case object UseLog1p extends NoArgWarning("Use math.log1p(x) instead of math.log(1 + x) for added accuracy when x is near 0")
case object UseExpm1 extends NoArgWarning("Use math.expm1(x) instead of math.exp(x) - 1 for added accuracy when x is near 0.")
case class UnlikelyEquality(lhs: String, rhs: String) extends
  TwoArgWarning("Comparing with == on instances of different types (%s, %s) will probably return false.", lhs, rhs) {
  def name = "UnlikelyEquality"
}
case object UseHypot extends NoArgWarning("Use math.hypot(x, y), instead of sqrt(x^2, y^2) for improved accuracy (but diminished performance).")
case object UseCbrt extends NoArgWarning("Use math.cbrt(x), instead of pow(x, 1/3) for improved accuracy and performance.")
case object UseSqrt extends NoArgWarning("Use math.sqrt(x), instead of pow(x, 1/2) for improved accuracy and performance.")
case object UseExp extends NoArgWarning("Use math.exp(x), instead of pow(E, x) for improved performance.")
case object UseLog10 extends NoArgWarning("Use math.log10(x), instead of log(x)/log(10) for improved accuracy.")
case object UseAbsNotSqrtSquare extends NoArgWarning("Use abs instead of sqrt(x^2).")
case object UseIsNanNotSelfComparison extends NoArgWarning("Use .isNan instead of comparing to itself.")
case object UseIsNanNotNanComparison extends NoArgWarning("Use .isNan instead of comparing to NaN, which is wrong.")
case object UseSignum extends NoArgWarning("Did you mean to use the signum function here? (signum also avoids division by zero exceptions)")
case object BigDecimalNumberFormat extends NoArgWarning("This BigDecimal constructor will likely throw a NumberFormatException.")
case object BigDecimalPrecisionLoss extends NoArgWarning("Possible loss of precision - use a string constant")
case object ReflexiveAssignment extends NoArgWarning("Assigning a variable to itself?")
case object CloseSourceFile extends NoArgWarning("You should close the file stream after use.")
case object JavaConverters extends NoArgWarning("Consider using the explicit collection.JavaConverters instead of implicit conversions in collection.JavaConversions.")
case class ContainsTypeMismatch(seqType: String, targetType: String) extends
  TwoArgWarning("%s.contains(%s) will probably return false because the collection and target element are of different types.", seqType, targetType) {
  def name = "ContainsTypeMatch"
}
case class NumberInstanceOf(tpe: String) extends TwoArgWarning("Use to%s instead of asInstanceOf[%s].", tpe, tpe) {
  def name = "NumberInstanceOf"
}
case object PatternMatchConstant extends NoArgWarning("Pattern matching on a constant value.")
case object PreferIfToBooleanMatch extends NoArgWarning("This is probably better written as an if statement.")
case class IdenticalCaseBodies(n: String) extends OneArgWarning("Bodies of %s neighbouring cases are identical and could be merged.", n) {
  def name = "IdenticalCaseBodies"
}
case object IdenticalCaseConditions extends NoArgWarning("Identical case condition detected above. This case will never match.")
case object ReflexiveComparison extends NoArgWarning("Same expression on both sides of comparison.")
case object YodaConditions extends NoArgWarning("You are using Yoda conditions")
case class UseConditionDirectly(negated: Boolean = false) extends OneArgWarning("Remove the if and just use the %scondition.", if (negated) "negated " else "") {
  def name = "UseConditionDirectly"
}
case class UseIfExpression(varName: String) extends OneArgWarning("Assign the result of the if expression to variable %s directly.", varName) {
  def name = "UseIfExpression"
}
case object UnnecessaryElseBranch extends NoArgWarning("This else branch is unnecessary, as the then branch always returns.")
case object DuplicateIfBranches extends NoArgWarning("If statement branches have the same structure.")
case object IdenticalIfElseCondition extends NoArgWarning("This condition has appeared earlier in the if-else chain and will never hold here.")
case object MergeNestedIfs extends NoArgWarning("These two nested ifs can be merged into one.")
case class VariableAssignedUnusedValue(variableName: String) extends OneArgWarning("Variable %s has an unused value before this reassign.", variableName) {
  def name = "VariableAssignedUnusedValue"
}
case object MalformedSwap extends NoArgWarning("Did you mean to swap these two variables?")
case object IdenticalIfCondition extends NoArgWarning("Two subsequent ifs have the same condition")
case object IdenticalStatements extends NoArgWarning("You're doing the exact same thing twice or more.")
case object IndexingWithNegativeNumber extends NoArgWarning("Using a negative index for a collection.")
case object OptionOfOption extends NoArgWarning("Why would you need an Option of an Option?")
case class UndesirableTypeInference(inferredType: String) extends OneArgWarning("Inferred type %s. This might not be what you intended. Add explicit type if that's what you want.", inferredType) {
  def name = "UndesirableTypeInference"
}
case object AssigningOptionToNull extends NoArgWarning("You probably meant None, not null.")
case object WrapNullWithOption extends NoArgWarning("Use Option(...), which automatically wraps null to None.")
case object UseGetOrElseOnOption extends NoArgWarning("Use getOrElse(...) instead of orElse(Some(...)).get.")
case class UseOptionOrNull(insteadOf: String) extends OneArgWarning("Use opt.orNull or opt.getOrElse(null) instead of if(%s) opt.get else null.", insteadOf) {
  def name = "UseOptionOrNull"
}
case class UseOptionGetOrElse(insteadOf: String) extends OneArgWarning("Use opt.getOrElse(...) instead of if(%s) opt.get else ...", insteadOf) {
  def name = "UseOptionGetOrElse"
}
case class UseExistsOnOption(findFilter: String, isEmptyisDefined: String) extends TwoArgWarning("Use exists(...) instead of %s(...).%s.", findFilter, isEmptyisDefined) {
  def name = "UseExistsOnOption"
}
case object UseFilterNotFlatMap extends NoArgWarning("Use filter(x => condition) instead of flatMap(x => if(condition) ... else ...).")
case object AvoidOptionStringSize extends NoArgWarning("Did you mean to take the size of the string inside the Option?")
case object AvoidOptionCollectionSize extends NoArgWarning("Did you mean to take the size of the collection inside the Option?")
case class AvoidOptionMethod(method: String, explanation: String = "") extends TwoArgWarning("Using Option.%s is not recommended. %s", method, explanation) {
  def name = "AvoidOptionMethod"
}
case object TransformNotMap extends NoArgWarning("Use x.transform(...) instead of x = x.map(...).")
case object DuplicateKeyInMap extends NoArgWarning("This key has already been defined, and will override the previous mapping.")
case class InefficientUseOfListSize(replacement: String) extends OneArgWarning("Use %s instead of comparing to List.size.", replacement) {
  def name = "InefficientUseOfListSize"
}
case object OnceEvaluatedStatementsInBlockReturningFunction extends NoArgWarning("You're passing a block that returns a function. The statements in this block, except the last one, will only be executed once.")
case object IntDivisionAssignedToFloat extends NoArgWarning("Integer division detected in an expression assigned to a floating point variable.")
case object UseFlattenNotFilterOption extends NoArgWarning("Use col.flatten instead of col.filter(_.isDefined).map(_.get).")
case class PassPartialFunctionDirectly(matchVar: String) extends OneArgWarning("You can pass the partial function in directly. (Remove \"%s match {\").", matchVar) {
  def name = "PassPartialFunctionDirectly"
}
case class UnitImplicitOrdering(function: String) extends OneArgWarning("Unit is returned here, so this %s will always return the first element.", function) {
  def name = "UnitImplicitOrdering"
}
case class RegexWarning(errorMessage: String, error: Boolean = true) extends OneArgWarning("Regex pattern "+(if(error) "syntax error" else "warning")+": %s", errorMessage) {
  def name = "RegexWarning"
}
case class InvariantCondition(always: Boolean, doWhat: String) extends TwoArgWarning("This condition will %s %s.", if (always) "always" else "never", doWhat) {
  def name = "InvariantCondition"
}
case class DecomposingEmptyCollection(method: String, collection: String = "collection") extends TwoArgWarning("Taking the %s of an empty %s.", method, collection) {
  def name = "DecomposingEmptyCollection"
}
case class InvariantExtrema(max: Boolean, returnsFirst: Boolean) extends TwoArgWarning("This %s will always return the %s value", if (max) "max" else "min", if (returnsFirst) "first" else "second") {
  def name = "InvariantExtrema"
}
case class UnnecessaryMethodCall(method: String) extends OneArgWarning("This %s is always unnecessary.", method) {
  def name = "UnnecessaryMethodCall"
}
case object ProducesEmptyCollection extends NoArgWarning("The resulting collection will always be empty.")
case class OperationAlwaysProducesZero(operation: String) extends OneArgWarning("Same values on both sides of %s will return 0.", operation) {
  def name = "OperationAlwaysProducesZero"
}
case object ModuloByOne extends NoArgWarning("Taking the modulo by one will return zero.")
case object DivideByOne extends NoArgWarning("Dividing by one will return the original number.")
case object DivideByZero extends NoArgWarning("Possible division by zero.")
case object ZeroDivideBy extends NoArgWarning("Division of zero will return zero.")
case object UseUntilNotToMinusOne extends NoArgWarning("Use (low until high) instead of (low to high-1).")
case object InvalidParamToRandomNextInt extends NoArgWarning("The parameter of this nextInt might be lower than 1 here.")
case object UnusedForLoopIteratorValue extends NoArgWarning("Iterator value is not used in the for loop's body.")
case object StringMultiplicationByNonPositive extends NoArgWarning("Multiplying a string with a value <= 0 will result in an empty string.")
case class LikelyIndexOutOfBounds(direction: String) extends OneArgWarning("You will likely use a %s index.", direction) {
  def name = "LikelyIndexOutOfBounds"
}
case object UnnecessaryReturn extends NoArgWarning("Scala has implicit return; you don't need a return statement at the end of a method.")
case class InvariantReturn(structure: String, returnValue: String) extends TwoArgWarning("This %s always returns the same value: %s", structure, returnValue) {
  def name = "InvariantReturn"
}
case class UnusedParameter(parameters: Seq[String], method: String) extends
  TwoArgWarning("Parameter%s not used in method %s.",
    parameters match { case Seq(p) => " " + p + " is" case _ => "s (%s) are".format(parameters.mkString(", ")) },
    method) {
  def name = "UnusedParameter"
}
case class InvalidStringFormat(errorMessage: String, exception: Boolean = true) extends OneArgWarning(if(exception) "This string format will throw: %s" else "%s", errorMessage) {
  def name = "InvalidStringFormat"
}
case class InvalidStringConversion(conversionType: String) extends OneArgWarning("This String %s conversion will likely fail.", conversionType) {
  def name = "InvalidStringConversion"
}
case object UnnecessaryStringNonEmpty extends NoArgWarning("This string will never be empty.")
case object UnnecessaryStringIsEmpty extends NoArgWarning("This string will always be empty.")
case class PossibleLossOfPrecision(improvement: String) extends OneArgWarning("Possible loss of precision. %s", improvement) {
  def name = "PossibleLossOfPrecision"
}
case class UnsafeAbs(improvement: String) extends OneArgWarning("Possibly unsafe use of abs. %s", improvement) {
  def name = "UnsafeAbs"
}
case class TypeToType(tpe: String) extends TwoArgWarning("Using to%s on something that is already of type %s", tpe, tpe) {
  def name = "TypeToType"
}
case object EmptyStringInterpolator extends NoArgWarning("This string interpolation has no arguments.")
case class UnlikelyToString(tpe: String) extends OneArgWarning("Using toString on type %s is likely unintended.", tpe) {
  def name = "UnlikelyToString"
}
case object UnthrownException extends NoArgWarning("This exception is likely meant to be thrown here.")
case object SuspiciousMatches extends NoArgWarning("This regex starts with ^ or ends with $. The matches method always matches the entire string.")
case object UseFindNotFilterHead extends NoArgWarning("Unless there are side-effects, .filter(...).headOption can be replaced by .find(...).")
case object IfDoWhile extends NoArgWarning("The if and the do-while loop have the same condition. Use a while loop.")
