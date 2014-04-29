package com.foursquare.lint

import org.specs2.matcher.JUnitMustMatchers
import org.junit.Test

class LinterOptionsTest extends JUnitMustMatchers {
  @Test
  def noOptionsResultsInNoneDisabled() {
    val Right(linterOptions) = LinterOptions.parse(Nil)
    val nonUnitResult = linterOptions.disabledWarningNames.isEmpty must beTrue
  }

  @Test
  def disableOnlyTwoWarning() {
    val Right(linterOptions) = LinterOptions.parse(List("disable:UseLog1p+UseExpm1"))
    val nonUnitResult = linterOptions.disabledWarningNames.toList must be_==(List("UseLog1p", "UseExpm1"))
  }

  @Test
  def enableOnlyTwoWarning() {
    val Right(linterOptions) = LinterOptions.parse(List("enable-only:UseExpm1+UseLog1p"))
    val nonUnitResult = (Warning.AllNames.toSet -- linterOptions.disabledWarningNames).toList.sorted must be_==(List("UseExpm1", "UseLog1p"))
  }
}
