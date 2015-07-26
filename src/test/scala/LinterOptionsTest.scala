package org.psywerx.hairyfotr

import org.junit.Test
import org.specs2.matcher.JUnitMustMatchers

class LinterOptionsTest extends JUnitMustMatchers {
  @Test
  def noOptionsResultsInNoneDisabled(): Unit = {
    val Right(linterOptions) = LinterOptions.parse(Nil)
    val nonUnitResult = linterOptions.disabledWarningNames.isEmpty must beTrue
  }

  @Test
  def disableOnlyTwoWarning(): Unit = {
    val Right(linterOptions) = LinterOptions.parse(List("disable:UseLog1p+UseExpm1"))
    val nonUnitResult = linterOptions.disabledWarningNames.toList must be_==(List("UseLog1p", "UseExpm1"))
  }

  @Test
  def enableOnlyTwoWarning(): Unit = {
    val Right(linterOptions) = LinterOptions.parse(List("enable-only:UseExpm1+UseLog1p"))
    val nonUnitResult = (Warning.AllNames.toSet -- linterOptions.disabledWarningNames).toList.sorted must be_==(List("UseExpm1", "UseLog1p"))
  }

  @Test
  def printWarningNames(): Unit = {
    val Right(linterOptions) = LinterOptions.parse(List("printWarningNames"))
    val nonUnitResult = linterOptions.printWarningNames must beTrue
  }
  @Test
  def printWarningNamesTrue(): Unit = {
    val Right(linterOptions) = LinterOptions.parse(List("printWarningNames:true"))
    val nonUnitResult = linterOptions.printWarningNames must beTrue
  }
  @Test
  def printWarningNamesFalse(): Unit = {
    val Right(linterOptions) = LinterOptions.parse(List("printWarningNames:false"))
    val nonUnitResult = linterOptions.printWarningNames must beFalse
  }
}
