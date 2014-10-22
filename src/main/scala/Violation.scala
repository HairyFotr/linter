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

sealed trait Violation {
  def message: String
  def name: String
}
// scalastyle:off public.methods.have.type
object Violation {
  final val All: Seq[Violation] = Vector(
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
    new RegexViolation("", false),
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

  final val NameToWarning: Map[String, Violation] = All.map(w => w.name -> w).toMap
}

sealed trait DefaultNameViolation extends Violation {
  def name = toString
}

sealed abstract class NoArgViolation(val message: String) extends DefaultNameViolation

sealed abstract class OneArgViolation(format: String, arg: String) extends Violation {
  def message = format.format(arg)
}
sealed abstract class TwoArgViolation(format: String, arg1: String, arg2: String) extends Violation {
  def message = format.format(arg1, arg2)
}

case object UnextendedSealedTrait extends NoArgViolation("This sealed trait is never extended")
case object UseLog1p extends NoArgViolation("Use math.log1p(x) instead of math.log(1 + x) for added accuracy when x is near 0")
case object UseExpm1 extends NoArgViolation("Use math.expm1(x) instead of math.exp(x) - 1 for added accuracy when x is near 0.")
case class UnlikelyEquality(lhs: String, rhs: String) extends
  TwoArgViolation("Comparing with == on instances of different types (%s, %s) will probably return false.", lhs, rhs) {
  def name = "UnlikelyEquality"
}
case object UseHypot extends NoArgViolation("Use math.hypot(x, y), instead of sqrt(x^2, y^2) for improved accuracy (but diminished performance).")
case object UseCbrt extends NoArgViolation("Use math.cbrt(x), instead of pow(x, 1/3) for improved accuracy and performance.")
case object UseSqrt extends NoArgViolation("Use math.sqrt(x), instead of pow(x, 1/2) for improved accuracy and performance.")
case object UseExp extends NoArgViolation("Use math.exp(x), instead of pow(E, x) for improved performance.")
case object UseLog10 extends NoArgViolation("Use math.log10(x), instead of log(x)/log(10) for improved accuracy.")
case object UseAbsNotSqrtSquare extends NoArgViolation("Use abs instead of sqrt(x^2).")
case object UseIsNanNotSelfComparison extends NoArgViolation("Use .isNan instead of comparing to itself.")
case object UseIsNanNotNanComparison extends NoArgViolation("Use .isNan instead of comparing to NaN, which is wrong.")
case object UseSignum extends NoArgViolation("Did you mean to use the signum function here? (signum also avoids division by zero exceptions)")
case object BigDecimalNumberFormat extends NoArgViolation("This BigDecimal constructor will likely throw a NumberFormatException.")
case object BigDecimalPrecisionLoss extends NoArgViolation("Possible loss of precision - use a string constant")
case object ReflexiveAssignment extends NoArgViolation("Assigning a variable to itself?")
case object CloseSourceFile extends NoArgViolation("You should close the file stream after use.")
case object JavaConverters extends NoArgViolation("Consider using the explicit collection.JavaConverters instead of implicit conversions in collection.JavaConversions.")
case class ContainsTypeMismatch(seqType: String, targetType: String) extends
  TwoArgViolation("%s.contains(%s) will probably return false because the collection and target element are of different types.", seqType, targetType) {
  def name = "ContainsTypeMatch"
}
case class NumberInstanceOf(tpe: String) extends TwoArgViolation("Use to%s instead of asInstanceOf[%s].", tpe, tpe) {
  def name = "NumberInstanceOf"
}
case object PatternMatchConstant extends NoArgViolation("Pattern matching on a constant value.")
case object PreferIfToBooleanMatch extends NoArgViolation("This is probably better written as an if statement.")
case class IdenticalCaseBodies(n: String) extends OneArgViolation("Bodies of %s neighbouring cases are identical and could be merged.", n) {
  def name = "IdenticalCaseBodies"
}
case object IdenticalCaseConditions extends NoArgViolation("Identical case condition detected above. This case will never match.")
case object ReflexiveComparison extends NoArgViolation("Same expression on both sides of comparison.")
case object YodaConditions extends NoArgViolation("You are using Yoda conditions")
case class UseConditionDirectly(negated: Boolean = false) extends OneArgViolation("Remove the if and just use the %scondition.", if (negated) "negated " else "") {
  def name = "UseConditionDirectly"
}
case class UseIfExpression(varName: String) extends OneArgViolation("Assign the result of the if expression to variable %s directly.", varName) {
  def name = "UseIfExpression"
}
case object UnnecessaryElseBranch extends NoArgViolation("This else branch is unnecessary, as the then branch always returns.")
case object DuplicateIfBranches extends NoArgViolation("If statement branches have the same structure.")
case object IdenticalIfElseCondition extends NoArgViolation("This condition has appeared earlier in the if-else chain and will never hold here.")
case object MergeNestedIfs extends NoArgViolation("These two nested ifs can be merged into one.")
case class VariableAssignedUnusedValue(variableName: String) extends OneArgViolation("Variable %s has an unused value before this reassign.", variableName) {
  def name = "VariableAssignedUnusedValue"
}
case object MalformedSwap extends NoArgViolation("Did you mean to swap these two variables?")
case object IdenticalIfCondition extends NoArgViolation("Two subsequent ifs have the same condition")
case object IdenticalStatements extends NoArgViolation("You're doing the exact same thing twice or more.")
case object IndexingWithNegativeNumber extends NoArgViolation("Using a negative index for a collection.")
case object OptionOfOption extends NoArgViolation("Why would you need an Option of an Option?")
case class UndesirableTypeInference(inferredType: String) extends OneArgViolation("Inferred type %s. This might not be what you intended. Add explicit type if that's what you want.", inferredType) {
  def name = "UndesirableTypeInference"
}
case object AssigningOptionToNull extends NoArgViolation("You probably meant None, not null.")
case object WrapNullWithOption extends NoArgViolation("Use Option(...), which automatically wraps null to None.")
case object UseGetOrElseOnOption extends NoArgViolation("Use getOrElse(...) instead of orElse(Some(...)).get.")
case class UseOptionOrNull(insteadOf: String) extends OneArgViolation("Use opt.orNull or opt.getOrElse(null) instead of if(%s) opt.get else null.", insteadOf) {
  def name = "UseOptionOrNull"
}
case class UseOptionGetOrElse(insteadOf: String) extends OneArgViolation("Use opt.getOrElse(...) instead of if(%s) opt.get else ...", insteadOf) {
  def name = "UseOptionGetOrElse"
}
case class UseExistsOnOption(findFilter: String, isEmptyisDefined: String) extends TwoArgViolation("Use exists(...) instead of %s(...).%s.", findFilter, isEmptyisDefined) {
  def name = "UseExistsOnOption"
}
case object UseFilterNotFlatMap extends NoArgViolation("Use filter(x => condition) instead of flatMap(x => if(condition) ... else ...).")
case object AvoidOptionStringSize extends NoArgViolation("Did you mean to take the size of the string inside the Option?")
case object AvoidOptionCollectionSize extends NoArgViolation("Did you mean to take the size of the collection inside the Option?")
case class AvoidOptionMethod(method: String, explanation: String = "") extends TwoArgViolation("Using Option.%s is not recommended. %s", method, explanation) {
  def name = "AvoidOptionMethod"
}
case object DuplicateKeyInMap extends NoArgViolation("This key has already been defined, and will override the previous mapping.")
case class InefficientUseOfListSize(replacement: String) extends OneArgViolation("Use %s instead of comparing to List.size.", replacement) {
  def name = "InefficientUseOfListSize"
}
case object OnceEvaluatedStatementsInBlockReturningFunction extends NoArgViolation("You're passing a block that returns a function. The statements in this block, except the last one, will only be executed once.")
case object IntDivisionAssignedToFloat extends NoArgViolation("Integer division detected in an expression assigned to a floating point variable.")
case object UseFlattenNotFilterOption extends NoArgViolation("Use col.flatten instead of col.filter(_.isDefined).map(_.get).")
case class PassPartialFunctionDirectly(matchVar: String) extends OneArgViolation("You can pass the partial function in directly. (Remove \"%s match {\").", matchVar) {
  def name = "PassPartialFunctionDirectly"
}
case class UnitImplicitOrdering(function: String) extends OneArgViolation("Unit is returned here, so this %s will always return the first element.", function) {
  def name = "UnitImplicitOrdering"
}
case class RegexViolation(errorMessage: String, error: Boolean = true) extends OneArgViolation("Regex pattern "+(if(error) "syntax error" else "warning")+": %s", errorMessage) {
  def name = "RegexWarning"
}
case class InvariantCondition(always: Boolean, doWhat: String) extends TwoArgViolation("This condition will %s %s.", if (always) "always" else "never", doWhat) {
  def name = "InvariantCondition"
}
case class DecomposingEmptyCollection(method: String, collection: String = "collection") extends TwoArgViolation("Taking the %s of an empty %s.", method, collection) {
  def name = "DecomposingEmptyCollection"
}
case class InvariantExtrema(max: Boolean, returnsFirst: Boolean) extends TwoArgViolation("This %s will always return the %s value", if (max) "max" else "min", if (returnsFirst) "first" else "second") {
  def name = "InvariantExtrema"
}
case class UnnecessaryMethodCall(method: String) extends OneArgViolation("This %s is always unnecessary.", method) {
  def name = "UnnecessaryMethodCall"
}
case object ProducesEmptyCollection extends NoArgViolation("The resulting collection will always be empty.")
case class OperationAlwaysProducesZero(operation: String) extends OneArgViolation("Same values on both sides of %s will return 0.", operation) {
  def name = "OperationAlwaysProducesZero"
}
case object ModuloByOne extends NoArgViolation("Taking the modulo by one will return zero.")
case object DivideByOne extends NoArgViolation("Dividing by one will return the original number.")
case object DivideByZero extends NoArgViolation("Possible division by zero.")
case object ZeroDivideBy extends NoArgViolation("Division of zero will return zero.")
case object UseUntilNotToMinusOne extends NoArgViolation("Use (low until high) instead of (low to high-1).")
case object InvalidParamToRandomNextInt extends NoArgViolation("The parameter of this nextInt might be lower than 1 here.")
case object UnusedForLoopIteratorValue extends NoArgViolation("Iterator value is not used in the for loop's body.")
case object StringMultiplicationByNonPositive extends NoArgViolation("Multiplying a string with a value <= 0 will result in an empty string.")
case class LikelyIndexOutOfBounds(direction: String) extends OneArgViolation("You will likely use a %s index.", direction) {
  def name = "LikelyIndexOutOfBounds"
}
case object UnnecessaryReturn extends NoArgViolation("Scala has implicit return; you don't need a return statement at the end of a method.")
case class InvariantReturn(structure: String, returnValue: String) extends TwoArgViolation("This %s always returns the same value: %s", structure, returnValue) {
  def name = "InvariantReturn"
}
case class UnusedParameter(parameters: Seq[String], method: String) extends
  TwoArgViolation("Parameter%s not used in method %s.",
    parameters match { case Seq(p) => " " + p + " is" case _ => "s (%s) are".format(parameters.mkString(", ")) },
    method) {
  def name = "UnusedParameter"
}
case class InvalidStringFormat(errorMessage: String, exception: Boolean = true) extends OneArgViolation(if(exception) "This string format will throw: %s" else "%s", errorMessage) {
  def name = "InvalidStringFormat"
}
case class InvalidStringConversion(conversionType: String) extends OneArgViolation("This String %s conversion will likely fail.", conversionType) {
  def name = "InvalidStringConversion"
}
case object UnnecessaryStringNonEmpty extends NoArgViolation("This string will never be empty.")
case object UnnecessaryStringIsEmpty extends NoArgViolation("This string will always be empty.")
case class PossibleLossOfPrecision(improvement: String) extends OneArgViolation("Possible loss of precision. %s", improvement) {
  def name = "PossibleLossOfPrecision"
}
case class UnsafeAbs(improvement: String) extends OneArgViolation("Possibly unsafe use of abs. %s", improvement) {
  def name = "UnsafeAbs"
}
case class TypeToType(tpe: String) extends TwoArgViolation("Using to%s on something that is already of type %s", tpe, tpe) {
  def name = "TypeToType"
}
case object EmptyStringInterpolator extends NoArgViolation("This string interpolation has no arguments.")
case class UnlikelyToString(tpe: String) extends OneArgViolation("Using toString on type %s is likely unintended.", tpe) {
  def name = "UnlikelyToString"
}
case object UnthrownException extends NoArgViolation("This exception is likely meant to be thrown here.")
case object SuspiciousMatches extends NoArgViolation("This regex starts with ^ or ends with $. The matches method always matches the entire string.")
case object UseFindNotFilterHead extends NoArgViolation("Unless there are side-effects, .filter(...).headOption can be replaced by .find(...).")
case object IfDoWhile extends NoArgViolation("The if and the do-while loop have the same condition. Use a while loop.")
