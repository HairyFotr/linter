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

package org.psywerx.hairyfotr

sealed abstract class Warning(val message: String) {
  def name: String = toString.takeWhile(_ != '(')
}

object Warning {
  final val All: Seq[Warning] = Vector(
    AssigningOptionToNull,
    AvoidOptionCollectionSize,
    AvoidOptionMethod("", ""),
    AvoidOptionStringSize,
    BigDecimalNumberFormat,
    BigDecimalPrecisionLoss,
    CloseSourceFile,
    ContainsTypeMismatch("", ""),
    NumberInstanceOf(""),
    DecomposingEmptyCollection("", ""),
    ModuloByOne,
    DivideByOne,
    DivideByZero,
    ZeroDivideBy,
    DuplicateIfBranches,
    DuplicateKeyInMap,
    TransformNotMap(""),
    IdenticalCaseBodies(""),
    IdenticalCaseConditions,
    IdenticalIfCondition,
    IdenticalIfElseCondition,
    IdenticalStatements,
    IndexingWithNegativeNumber,
    InefficientUseOfListSize("", "", ""),
    IntDivisionAssignedToFloat,
    InvalidParamToRandomNextInt,
    InvalidStringConversion(""),
    InvalidStringFormat(""),
    InvariantCondition(true, ""),
    InvariantExtrema(true, true),
    InvariantReturn("", ""),
    JavaConverters,
    LikelyIndexOutOfBounds(""),
    MalformedSwap,
    MergeNestedIfs,
    OnceEvaluatedStatementsInBlockReturningFunction,
    OperationAlwaysProducesZero(""),
    OptionOfOption,
    PassPartialFunctionDirectly(""),
    UnitImplicitOrdering(""),
    PatternMatchConstant,
    PossibleLossOfPrecision(""),
    PreferIfToBooleanMatch,
    ProducesEmptyCollection,
    ReflexiveAssignment,
    ReflexiveComparison,
    RegexWarning("", true),
    StringMultiplicationByNonPositive,
    UndesirableTypeInference(""),
    UnextendedSealedTrait,
    UnlikelyEquality("lhs", "rhs", "=="),
    UnnecessaryMethodCall(""),
    UnnecessaryReturn,
    UnnecessaryStringIsEmpty,
    UnnecessaryStringNonEmpty,
    UnusedForLoopIteratorValue,
    UnusedParameter(Seq("unusedParameter"), "methodName"),
    UseHypot,
    UseLog1p,
    UseLog10,
    UseCbrt,
    UseSqrt,
    UseExp,
    UseExpm1,
    UseAbsNotSqrtSquare,
    UseConditionDirectly(true),
    UseIfExpression(""),
    MergeMaps,
    FuncFirstThenMap(""),
    FilterFirstThenSort,
    UseMinOrMaxNotSort("", "", "", ""),
    UseMapNotFlatMap(""),
    UseFilterNotFlatMap(""),
    UseFlattenNotFilterOption(""),
    UseExistsNotFilterEmpty("", true),
    UseCountNotFilterLength("", ""),
    UseExistsNotCountCompare(""),
    UseGetOrElseOnOption(""),
    UseExistsNotFindIsDefined("", ""),
    UseExistsNotFilterIsEmpty("", "", ""),
    UseFindNotFilterHead(""),
    UseContainsNotExistsEquals("", "", "", ""),
    UseQuantifierFuncNotFold("", "", ""),
    UseFuncNotReduce("", "", ""),
    UseFuncNotFold("", "", ""),
    UseIsNanNotNanComparison,
    UseIsNanNotSelfComparison,
    UseOptionGetOrElse("", ""),
    UseOptionOrNull("", ""),
    UseSignum,
    UseUntilNotToMinusOne,
    VariableAssignedUnusedValue(""),
    WrapNullWithOption,
    YodaConditions,
    UnsafeAbs(""),
    TypeToType(""),
    EmptyStringInterpolator,
    UnlikelyToString(""),
    UnthrownException,
    SuspiciousMatches,
    IfDoWhile)

  final val AllNames = All.map(_.name)

  final val NameToWarning: Map[String, Warning] = All.map(w => w.name -> w).toMap
}

case object UnextendedSealedTrait extends
  Warning("This sealed trait is never extended")
case class UnlikelyEquality(lhs: String, rhs: String, op: String) extends
  Warning(s"Comparing with $op on instances of unrelated types ${(lhs, rhs).toString}.")
case object UseLog1p extends
  Warning("Use math.log1p(x), instead of math.log(1 + x) for added accuracy when x is near 0.")
case object UseLog10 extends
  Warning("Use math.log10(x), instead of log(x)/log(10) for improved accuracy.")
case object UseExpm1 extends
  Warning("Use math.expm1(x), instead of math.exp(x) - 1 for added accuracy when x is near 0.")
case object UseHypot extends
  Warning("Use math.hypot(x, y), instead of sqrt(x^2 + y^2) for improved accuracy (but diminished performance).")
case object UseCbrt extends
  Warning("Use math.cbrt(x), instead of pow(x, 1/3) for improved accuracy and performance.")
case object UseSqrt extends
  Warning("Use math.sqrt(x), instead of pow(x, 1/2) for improved accuracy and performance.")
case object UseExp extends
  Warning("Use math.exp(x), instead of pow(E, x) for improved performance.")
case object UseAbsNotSqrtSquare extends
  Warning("Use math.abs(x) instead of math.sqrt(x^2).")
case object UseIsNanNotSelfComparison extends
  Warning("Use x.isNan instead of comparing x to itself.") // Turn all these x-es into varName
case object UseIsNanNotNanComparison extends
  Warning("Use x.isNan instead of comparing it to NaN (NaN == NaN is false, so this is always wrong).") // Error
case object UseSignum extends
  Warning("Did you mean to use the signum function here? (signum also avoids division by zero exceptions).")
case object BigDecimalNumberFormat extends
  Warning("This BigDecimal constructor will likely throw a NumberFormatException.") // Likely Exception
case object BigDecimalPrecisionLoss extends
  Warning("Possible loss of precision - use a string constant.")
case object ReflexiveAssignment extends
  Warning("Assigning a variable to itself?")
case object CloseSourceFile extends
  Warning("You should close the file stream after use. (Streams get garbage collected, but it's possible to open too many at once)")
case object JavaConverters extends
  Warning("Consider using the explicit collection.JavaConverters instead of implicit conversions in collection.JavaConversions.")
case class ContainsTypeMismatch(seqType: String, targetType: String) extends 
  Warning(s"$seqType.contains($targetType) will probably return false, since the collection and target element are of unrelated types.")
case class NumberInstanceOf(tpe: String) extends
  Warning(s"Use to$tpe instead of asInstanceOf[$tpe].")
case object PatternMatchConstant extends
  Warning("Pattern matching on a constant value.")
case object PreferIfToBooleanMatch extends
  Warning("Pattern matching on Boolean is probably better written as an if statement.")
case class IdenticalCaseBodies(n: String) extends
  Warning(s"Bodies of $n neighbouring cases are identical and could be merged.")
case object IdenticalCaseConditions extends
  Warning("Identical case condition detected above. This case will never match.") // Why doesn't compiler catch this?
case object ReflexiveComparison extends
  Warning("Same expression on both sides of the comparison.")
case object YodaConditions extends
  Warning("Yoda conditions using you are.")
case class UseConditionDirectly(negated: Boolean = false) extends
  Warning(s"""Remove the if expression and use the ${if (negated) "negated " else ""}condition directly.""")
case class UseIfExpression(varName: String) extends
  Warning(s"Assign the result of the if expression to variable $varName directly.")
case object UnnecessaryElseBranch extends
  Warning("This else branch is unnecessary, as the then branch always returns.")
case object DuplicateIfBranches extends
  Warning("If statement branches have the same structure.")
case object IdenticalIfElseCondition extends
  Warning("This condition has appeared earlier in the if-else chain and will never hold here. (except for side-effecting conditions)")
case object MergeNestedIfs extends
  Warning("These two nested ifs can be merged into one.")
case class VariableAssignedUnusedValue(varName: String) extends
  Warning(s"Variable $varName has an unused value before this reassign.")
case object MalformedSwap extends
  Warning("Did you mean to swap these two variables?")
case object IdenticalIfCondition extends
  Warning("Two subsequent ifs have the same condition. (check if value has changed in between)")
case object IdenticalStatements extends
  Warning("You're doing the exact same thing twice or more.")
case object IndexingWithNegativeNumber extends
  Warning("Using a negative index for a collection.")
case object OptionOfOption extends
  Warning("Why would you need an Option of an Option?")
case class UndesirableTypeInference(inferredType: String) extends
  Warning(s"Inferred type $inferredType. (This might not be what you've intended)")
case object AssigningOptionToNull extends
  Warning("You probably meant None, not null.")
case object WrapNullWithOption extends
  Warning("Use Option(...), which automatically wraps null to None.")
case object AvoidOptionStringSize extends
  Warning("Did you mean to take the size of the string inside the Option?")
case object AvoidOptionCollectionSize extends
  Warning("Did you mean to take the size of the collection inside the Option?")
case class UseGetOrElseOnOption(varname: String) extends
  Warning(s"Use $varname.getOrElse(...) instead of $varname.orElse(Some(...)).get.")
case class UseOptionOrNull(varname: String, insteadOf: String) extends
  Warning(s"Use $varname.orNull or $varname.getOrElse(null) instead of if ($insteadOf) $varname.get else null.")
case class UseOptionGetOrElse(varname: String, insteadOf: String) extends
  Warning(s"Use $varname.getOrElse(...) instead of if ($insteadOf) $varname.get else ...")
case class UseExistsNotFindIsDefined(varName: String, isEmpty_isDefined: String) extends
  Warning(s"Use $varName.exists(...) instead of $varName.find(...).$isEmpty_isDefined.")
case class UseExistsNotFilterIsEmpty(varName: String, filter: String, empty: String) extends
  Warning(s"Use $varName.exists(...) instead of $varName.$filter(...).$empty.")
case class UseFindNotFilterHead(varName: String) extends
  Warning(s"Unless there are side-effects, $varName.filter(...).headOption can be replaced by $varName.find(...).")
case class UseContainsNotExistsEquals(colName: String, valCmp: String, val1: String, val2: String) extends
  Warning(s"Use $colName.contains($valCmp) instead of $colName.exists($val1 == $val2)")
case class UseQuantifierFuncNotFold(varName: String, method: String, func: String) extends
  Warning(s"Unless there are side-effects, this $varName.$func can be replaced by $varName.$method.")
case class UseFuncNotReduce(varName: String, f: String, func: String) extends
  Warning(s"Use $varName.$f instead of $varName.$func.")
case class UseFuncNotFold(varName: String, f: String, func: String) extends
  Warning(s"Use $varName.$f instead of $varName.$func.")
case object MergeMaps extends
  Warning("Merge these two map operations.")
case class FuncFirstThenMap(methName: String) extends
  Warning(s"Use method $methName first, then map.")
case object FilterFirstThenSort extends
  Warning("Filter collection first, then sort it.")
case class UseMinOrMaxNotSort(colName: String, sortFunc: String, op: String, replacement: String) extends
  Warning(s"Use $colName.$replacement instead of $colName.$sortFunc.$op.")
case class UseMapNotFlatMap(varName: String) extends
  Warning(s"Use $varName.map(x => if (...) y else z) instead of $varName.flatMap(x => if (...) Collection(y) else Collection(z)).") // Clean up warning
case class UseFilterNotFlatMap(varName: String) extends
  Warning(s"Use $varName.filter(x => condition) instead of $varName.flatMap(x => if (condition) ... else ...).") // Clean up warning
case class AvoidOptionMethod(method: String, explanation: String = "") extends
  Warning(s"Using Option.$method is not recommended. $explanation")
case class TransformNotMap(varName: String) extends
  Warning(s"Use $varName.transform(...) instead of col = $varName.map(...).")
case object DuplicateKeyInMap extends
  Warning("This key has already been defined, and will override the previous mapping.")
case class InefficientUseOfListSize(varName: String, replacement: String, func: String) extends
  Warning(s"Use $varName.$replacement instead of comparing to $varName.$func. ($varName is a List, $func takes O(n) time)") // Cover length
case object OnceEvaluatedStatementsInBlockReturningFunction extends
  Warning("You're passing a block that returns a function. The statements in this block, except the last one, will only be executed once.")
case object IntDivisionAssignedToFloat extends
  Warning("Integer division detected in an expression assigned to a floating point variable.")
case class UseFlattenNotFilterOption(varName: String) extends
  Warning(s"Use $varName.flatten instead of $varName.filter(_.isDefined).map(_.get).") // Cover isEmpty, etc
case class UseExistsNotFilterEmpty(varName: String, bang: Boolean) extends
  Warning(s"""Use $varName.exists(...) instead of ${ if (bang) s"!$varName.filter(...).isEmpty" else s"$varName.filter(...).nonEmpty" }""")
case class UseCountNotFilterLength(varName: String, func: String) extends
  Warning(s"Use $varName.count(...) instead of $varName.filter(...).$func")
case class UseExistsNotCountCompare(varName: String) extends
  Warning(s"Use $varName.exists(...) instead of $varName.count(...) compare.")
case class PassPartialFunctionDirectly(matchVar: String) extends
  Warning(s"You can pass the partial function in directly. (Remove `$matchVar match {`).")
case class UnitImplicitOrdering(function: String) extends
  Warning(s"Unit is returned here, so this $function will always return the first element.")
case class RegexWarning(errorMessage: String, error: Boolean = true) extends
  Warning("Regex pattern "+(if (error) "syntax error" else "warning")+s": $errorMessage.")
case class InvariantCondition(always: Boolean, doWhat: String) extends
  Warning(s"""This condition will ${ if (always) "always" else "never" } $doWhat.""")
case class DecomposingEmptyCollection(method: String, collection: String = "collection") extends
  Warning(s"Taking the $method of an empty $collection.")
case class InvariantExtrema(max: Boolean, returnsFirst: Boolean) extends
  Warning(s"""This ${ if (max) "max" else "min" } will always return the ${ if (returnsFirst) "first" else "second" } value""")
case class UnnecessaryMethodCall(method: String) extends
  Warning(s"This $method is always unnecessary.")
case object ProducesEmptyCollection extends
  Warning("The resulting collection will always be empty.")
case class OperationAlwaysProducesZero(operation: String) extends
  Warning(s"Same values on both sides of $operation will return 0.")
case object ModuloByOne extends
  Warning("Taking the modulo by one will return zero.")
case object DivideByOne extends
  Warning("Dividing by one will return the original number.")
case object DivideByZero extends
  Warning("Possible division by zero.")
case object ZeroDivideBy extends
  Warning("Division of zero will return zero.")
case object UseUntilNotToMinusOne extends
  Warning("Use (low until high) instead of (low to high-1).")
case object InvalidParamToRandomNextInt extends
  Warning("The parameter of this .nextInt might be lower than 1 here.") // Likely Exception
case object UnusedForLoopIteratorValue extends
  Warning("Iterator value is not used in the for loop's body.")
case object StringMultiplicationByNonPositive extends
  Warning("Multiplying a string with a value <= 0 will result in an empty string.")
case class LikelyIndexOutOfBounds(direction: String) extends
  Warning(s"You will likely use a $direction index.")
case object UnnecessaryReturn extends
  Warning("Scala has implicit return. You don't need a return statement at the end of a method.")
case class InvariantReturn(structure: String, returnValue: String) extends
  Warning(s"This $structure always returns the same value: $returnValue.")
case class UnusedParameter(parameters: Seq[String], method: String) extends
  Warning("Parameter" +
    (parameters match { case Seq(p) => " " + p + " is" case _ => "s (" + parameters.mkString(", ") + ") are" }) +
    s" not used in method $method.")
case class InvalidStringFormat(errorMessage: String, exception: Boolean = true) extends
  Warning(if (exception) s"This string format will throw: $errorMessage" else s"$errorMessage") // Likely Exception
case class InvalidStringConversion(conversionType: String) extends
  Warning(s"This String $conversionType conversion will likely fail.") // Likely Exception
case object UnnecessaryStringNonEmpty extends
  Warning("This string will never be empty.")
case object UnnecessaryStringIsEmpty extends
  Warning("This string will always be empty.")
case class PossibleLossOfPrecision(improvement: String) extends
  Warning(s"Possible loss of precision. $improvement.")
case class UnsafeAbs(improvement: String) extends
  Warning(s"Possibly unsafe use of abs. $improvement.")
case class TypeToType(tpe: String) extends
  Warning(s"Using to$tpe on something that is already of type $tpe.")
case object EmptyStringInterpolator extends
  Warning("This string interpolation has no arguments.")
case class UnlikelyToString(tpe: String) extends
  Warning(s"Using toString on type $tpe is likely unintended.")
case object UnthrownException extends
  Warning("This exception was likely meant to be thrown here.")
case object SuspiciousMatches extends
  Warning("This regex starts with ^ or ends with $. The matches method always matches the entire string.")
case object IfDoWhile extends
  Warning("The if and the do-while loop have the same condition. Use a while loop.")

