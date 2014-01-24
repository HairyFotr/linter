package com.foursquare.lint

import org.specs2.matcher.JUnitMustMatchers
import org.junit.Test

class WarningTest extends JUnitMustMatchers {
  @Test
  def allIncludesAll() {
    val count: Int = Warning.All.distinct.map { (_: Warning) match {
      case AssigningOptionToNull => 1
      case AvoidOptionCollectionSize => 1
      case AvoidOptionSize => 1
      case AvoidOptionStringSize => 1
      case BigDecimalNumberFormat => 1
      case BigDecimalPrecisionLoss => 1
      case CloseSourceFile => 1
      case ContainsTypeMismatch(_,_) => 1
      case NumberInstanceOf(_) => 1
      case DecomposingEmptyCollection(_,_) => 1
      case DirectBooleanUse(_) => 1
      case DivideByOne => 1
      case DivisionByLiteralZero => 1
      case DuplicateIfBranches => 1
      case DuplicateKeyInMap => 1
      case IdenticalCaseBodies(_) => 1
      case IdenticalCaseConditions => 1
      case IdenticalIfCondition => 1
      case IdenticalIfElseCondition => 1
      case IdenticalStatements => 1
      case IndexingWithNegativeNumber => 1
      case InefficientUseOfListSize(_) => 1
      case IntDivisionAssignedToFloat => 1
      case InvalidParamToRandomNextInt => 1
      case InvalidStringConversion(_) => 1
      case InvalidStringFormat(_) => 1
      case InvariantCondition(_,_) => 1
      case InvariantExtrema(_,_) => 1
      case InvariantReturn(_,_) => 1
      case JavaConverters => 1
      case LikelyDivideByZero => 1
      case LikelyIndexOutOfBounds(_) => 1
      case MalformedSwap => 1
      case MergeNestedIfs => 1
      case ModuloByOne => 1
      case OnceEvaluatedStatementsInBlockReturningFunction => 1
      case OperationAlwaysProducesZero(_) => 1
      case OptionOfOption => 1
      case PassPartialFunctionDirectly(_) => 1
      case UnitImplicitOrdering(_) => 1
      case PatternMatchConstant => 1
      case PossibleLossOfPrecision(_) => 1
      case PreferIfToBooleanMatch => 1
      case ProducesEmptyCollection => 1
      case ReflexiveAssignment => 1
      case ReflexiveComparison => 1
      case RegexSyntaxError(_) => 1
      case StringMultiplicationByNonPositive => 1
      case UndesirableTypeInference(_) => 1
      case UnextendedSealedTrait => 1
      case UnlikelyEquality(_, _) => 1
      case UnnecessaryMethodCall(_) => 1
      case UnnecessaryReturn => 1
      case UnnecessaryStringIsEmpty => 1
      case UnnecessaryStringNonEmpty => 1
      case UnusedForLoopIteratorValue => 1
      case UnusedParameter(_,_) => 1
      case UseHypot => 1
      case UseCbrt => 1
      case UseLog10 => 1
      case UseAbsNotSqrtSquare => 1
      case UseConditionDirectly(_) => 1
      case UseIfExpression(_) => 1
      case UnnecessaryElseBranch => 1
      case UseExistsOnOption(_,_) => 1
      case UseExpm1 => 1
      case UseFilterNotFlatMap => 1
      case UseFlattenNotFilterOption => 1
      case UseGetOrElseOnOption => 1
      case UseIsNanNotNanComparison => 1
      case UseIsNanNotSelfComparison => 1
      case UseLog1p => 1
      case UseOptionGetOrElse(_) => 1
      case UseOptionOrNull(_) => 1
      case UseSignum => 1
      case UseUntilNotToMinusOne => 1
      case VariableAssignedUnusedValue(_) => 1
      case WrapNullWithOption => 1
      case YodaConditions => 1
      // ----------------------------------------------------------------------------------------------------------------------------------
      // If you get a warning here, it's likely because you added a new warning type but forgot to add it here. The real point is that you
      // need to add the new Warning to Warning.All.
      // ----------------------------------------------------------------------------------------------------------------------------------
    } }.sum
    val nonUnitResult = Warning.All.length must beEqualTo(count)
  }

  @Test
  def warningNamesAreAlphaNumeric() {
    Warning.AllNames.foreach { name =>
      name must beMatching("^[a-zA-Z0-9]+$")
    }
  }

  @Test
  def warningNamesAreUnique() {
    val warningsWithDuplicateNames = Warning.All.toList.groupBy(_.name).collect { case (_, warnings) if (warnings.length > 1) => warnings }
    warningsWithDuplicateNames.foreach(warnings => warnings.toString must beEqualTo("have duplicate names: " + warnings.head.name))
  }
}
