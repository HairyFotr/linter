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

object Warning {
  final val All: Seq[Warning] = Vector(
    AssigningOptionToNull,
    AvoidOptionCollectionSize,
    AvoidOptionSize,
    AvoidOptionStringSize,
    BigDecimalNumberFormat,
    BigDecimalPrecisionLoss,
    CloseSourceFile,
    new ContainsTypeMismatch("",""),
    new DecomposingEmptyCollection("",""),
    DivideByOne,
    DivisionByLiteralZero,
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
    LikelyDivideByZero,
    new LikelyIndexOutOfBounds(""),
    MalformedSwap,
    MergeNestedIfs,
    ModuloByOne,
    OnceEvaluatedStatementsInBlockReturningFunction,
    new OperationAlwaysProducesZero(""),
    OptionOfOption,
    new PassPartialFunctionDirectly(""),
    PatternMatchConstant,
    new PossibleLossOfPrecision(""),
    PreferIfToBooleanMatch,
    ProducesEmptyCollection,
    ReflexiveAssignment,
    ReflexiveComparison,
    new RegexSyntaxError(""),
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
    UseAbsNotSqrtPow,
    UseAbsNotSqrtSquare,
    new UseConditionDirectly(negated = false),
    new UseExistsOnOption("",""),
    UseExpm1,
    UseFilterNotFlatMap,
    UseFlattenNotFilterOption,
    UseGetOrElseOnOption,
    UseIsNanNotNanComparison,
    UseIsNanNotSelfComparison,
    UseLog1p,
    new UseOptionGetOrElse(""),
    new UseOptionOrNull(""),
    UseSigNum,
    UseUntilNotToMinusOne,
    new VariableAssignedUnusedValue(""),
    WrapNullWithOption,
    YodaConditions
  )

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
case class UnlikelyEquality(lhs: String, rhs: String) extends
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
case class ContainsTypeMismatch(seqType: String, targetType: String) extends
  TwoArgMessageWarning("%s.contains(%s) will probably return false because the collection and target element are of different types.", seqType, targetType) {
  def name = "ContainsTypeMatch"
}
case object PatternMatchConstant extends NoArgMessageWarning("Pattern matching on a constant value.")
case object PreferIfToBooleanMatch extends NoArgMessageWarning("This is probably better written as an if statement.")
case class IdenticalCaseBodies(n: String) extends OneArgMessageWarning("Bodies of %s neighbouring cases are identical and could be merged.", n) {
  def name = "IdenticalCaseBodies"
}
case object IdenticalCaseConditions extends NoArgMessageWarning("Identical case condition detected above. This case will never match.")
case object ReflexiveComparison extends NoArgMessageWarning("Same expression on both sides of comparison.")
case object YodaConditions extends NoArgMessageWarning("You are using Yoda conditions")
case class UseConditionDirectly(negated: Boolean = false) extends OneArgMessageWarning("Remove the if and just use the %scondition.", if (negated) "negated " else "") {
  def name = "UseConditionDirectly"
}
case object DuplicateIfBranches extends NoArgMessageWarning("If statement branches have the same structure.")
case object IdenticalIfElseCondition extends NoArgMessageWarning("This condition has appeared earlier in the if-else chain and will never hold here.")
case object MergeNestedIfs extends NoArgMessageWarning("These two nested ifs can be merged into one.")
case class VariableAssignedUnusedValue(variableName: String) extends OneArgMessageWarning("Variable %s has an unused value before this reassign.", variableName) {
  def name = "VariableAssignedUnusedValue"
}
case object MalformedSwap extends NoArgMessageWarning("Did you mean to swap these two variables?")
case object IdenticalIfCondition extends NoArgMessageWarning("Two subsequent ifs have the same condition")
case object IdenticalStatements extends NoArgMessageWarning("You're doing the exact same thing twice or more.")
case object IndexingWithNegativeNumber extends NoArgMessageWarning("Using a negative index for a collection.")
case object DivisionByLiteralZero extends NoArgMessageWarning("Literal division by zero.")
case object OptionOfOption extends NoArgMessageWarning("Why would you need an Option of an Option?")
case class UndesirableTypeInference(inferredType: String) extends OneArgMessageWarning("Inferred type %s. This might not be what you intended. Add explicit type if that's what you want.", inferredType) {
  def name = "UndesirableTypeInference"
}
case object AssigningOptionToNull extends NoArgMessageWarning("You probably meant None, not null.")
case object WrapNullWithOption extends NoArgMessageWarning("Use Option(...), which automatically wraps null to None.")
case object UseGetOrElseOnOption extends NoArgMessageWarning("Use getOrElse(...) instead of orElse(Some(...)).get.")
case class UseOptionOrNull(insteadOf: String) extends OneArgMessageWarning("Use opt.orNull or opt.getOrElse(null) instead of if(%s) opt.get else null.", insteadOf) {
  def name = "UseOptionOrNull"
}
case class UseOptionGetOrElse(insteadOf: String) extends OneArgMessageWarning("Use opt.getOrElse(...) instead of if(%s) opt.get else ...", insteadOf) {
  def name = "UseOptionGetOrElse"
}
case class UseExistsOnOption(findFilter: String, isEmptyisDefined: String) extends TwoArgMessageWarning("Use exists(...) instead of %s(...).%s.", findFilter, isEmptyisDefined) {
  def name = "UseExistsOnOption"
}
case object UseFilterNotFlatMap extends NoArgMessageWarning("Use filter(x => condition) instead of flatMap(x => if(condition) ... else ...).")
case object AvoidOptionStringSize extends NoArgMessageWarning("Did you mean to take the size of the string inside the Option?")
case object AvoidOptionCollectionSize extends NoArgMessageWarning("Did you mean to take the size of the collection inside the Option?")
case object AvoidOptionSize extends NoArgMessageWarning("Using Option.size is not recommended; use Option.isDefined instead.")
case object DuplicateKeyInMap extends NoArgMessageWarning("This key has already been defined, and will override the previous mapping.")
case class InefficientUseOfListSize(replacement: String) extends OneArgMessageWarning("Use %s instead of comparing to list.size.", replacement) {
  def name = "InefficientUseOfListSize"
}
case object OnceEvaluatedStatementsInBlockReturningFunction extends NoArgMessageWarning("You're passing a block that returns a function. The statements in this block, except the last one, will only be executed once.")
case object IntDivisionAssignedToFloat extends NoArgMessageWarning("Integer division detected in an expression assigned to a floating point variable.")
case object UseFlattenNotFilterOption extends NoArgMessageWarning("Use col.flatten instead of col.filter(_.isDefined).map(_.get).")
case class PassPartialFunctionDirectly(matchVar: String) extends OneArgMessageWarning("You can pass the partial function in directly. (Remove \"%s match {\").", matchVar) {
  def name = "PassPartialFunctionDirectly"
}
case class RegexSyntaxError(errorMessage: String) extends OneArgMessageWarning("Regex pattern syntax error: %s", errorMessage) {
  def name = "RegexSyntaxError"
}
case class InvariantCondition(always: Boolean, doWhat: String) extends TwoArgMessageWarning("This condition will %s %s.", if (always) "always" else "never", doWhat) {
  def name = "InvariantCondition"
}
case class DecomposingEmptyCollection(method: String, collection: String = "collection") extends TwoArgMessageWarning("Taking the %s of an empty %s.", method, collection) {
  def name = "DecomposingEmptyCollection"
}
case class InvariantExtrema(max: Boolean, returnsFirst: Boolean) extends TwoArgMessageWarning("This %s will always return the %s value", if (max) "max" else "min", if (returnsFirst) "first" else "second") {
  def name = "InvariantExtrema"
}
case class UnnecessaryMethodCall(method: String) extends OneArgMessageWarning("This %s is always unnecessary.", method) {
  def name = "UnnecessaryMethodCall"
}
case object ProducesEmptyCollection extends NoArgMessageWarning("The resulting collection will always be empty.")
case class OperationAlwaysProducesZero(operation: String) extends OneArgMessageWarning("Same values on both sides of %s will return 0.", operation) {
  def name = "OperationAlwaysProducesZero"
}
case object ModuloByOne extends NoArgMessageWarning("Taking the modulo by one will always return zero.")
case object DivideByOne extends NoArgMessageWarning("Dividing by one will always return the original number.")
case object LikelyDivideByZero extends NoArgMessageWarning("You will likely divide by zero here.")
case object UseUntilNotToMinusOne extends NoArgMessageWarning("Use (low until high) instead of (low to high-1).")
case object InvalidParamToRandomNextInt extends NoArgMessageWarning("The parameter of this nextInt might be lower than 1 here.")
case object UnusedForLoopIteratorValue extends NoArgMessageWarning("Iterator value is not used in the for loop's body.")
case object StringMultiplicationByNonPositive extends NoArgMessageWarning("Multiplying a string with a value <= 0 will result in an empty string.")
case class LikelyIndexOutOfBounds(direction: String) extends OneArgMessageWarning("You will likely use a %s index.", direction) {
  def name = "LikelyIndexOutOfBounds"
}
case object UnnecessaryReturn extends NoArgMessageWarning("Scala has implicit return; you don't need a return statement at the end of a method.")
case class InvariantReturn(structure: String, returnValue: String) extends TwoArgMessageWarning("This %s always returns the same value: %s", structure, returnValue) {
  def name = "InvariantReturn"
}
case class UnusedParameter(parameters: Seq[String], method: String) extends
  TwoArgMessageWarning("Parameter %s not used in method %s.",
    parameters match { case Seq(p) => p + " is" case _ => "s (%s) are".format(parameters.mkString(", ")) },
    method) {
  def name = "UnusedParameter"
}
case class InvalidStringFormat(errorMessage: String) extends OneArgMessageWarning("This string format will fail with: %s", errorMessage) {
  def name = "InvalidStringFormat"
}
case class InvalidStringConversion(conversionType: String) extends OneArgMessageWarning("This String %s conversion will likely fail.", conversionType) {
  def name = "InvalidStringConversion"
}
case object UnnecessaryStringNonEmpty extends NoArgMessageWarning("This string will never be empty.")
case object UnnecessaryStringIsEmpty extends NoArgMessageWarning("This string will always be empty.")
case class PossibleLossOfPrecision(improvement: String) extends OneArgMessageWarning("Possible loss of precision. %s", improvement) {
  def name = "PossibleLossOfPrecision"
}
