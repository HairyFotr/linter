package com.foursquare.lint

import org.specs2.matcher.JUnitMustMatchers
import org.junit.Test

class WarningTest extends JUnitMustMatchers {
  @Test 
  def allIncludesAll() {
    val count: Int = Warning.All.map { _ match { 
      case UnextendedSealedTrait | UseLog1p | UseExpm1 | UseAbsNotSqrtPow | UseAbsNotSqrtSquare | UseIsNanNotSelfComparison | UseIsNanNotNanComparison => 1
      case _ : UnlikelyEquality => 1
      // If you get a warning here, it's likely because you added a new warning type but forgot to add it here.
    } }.sum
    Warning.All.length must beEqualTo(count)
  }

  @Test
  def warningNamesAreAlphaNumeric() {
    Warning.AllNames.foreach { name =>
      name must beMatching("^[a-zA-Z0-9]+$")
    }
  }
}
