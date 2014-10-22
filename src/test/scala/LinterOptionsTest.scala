package com.foursquare.lint

import org.specs2.matcher.JUnitMustMatchers
import org.junit.Test

class LinterOptionsTest extends JUnitMustMatchers {

  @Test
  def noOptionsResultsInNoneDisabledAndLogLevelWarn() {
    // given
    val options = Nil

    // when
    val Right(linterOptions) = LinterOptions.parse(options)

    // then
    linterOptions.disabledWarningNames.isEmpty must beTrue
    linterOptions.notificationLevel must beTheSameAs(Warn)
  }

  @Test
  def disableOnlyTwoWarning() {
    // given
    val options = List("disable:UseLog1p+UseExpm1")

    // when
    val Right(linterOptions) = LinterOptions.parse(options)

    // then
    linterOptions.disabledWarningNames.toList must be_==(List("UseLog1p", "UseExpm1"))
  }

  @Test
  def setProperLogLevel(): Unit = {
    // given
    val warnOptions = List("level:warn")
    val errorOptions = List("level:error")

    // when
    val Right(warnLinterOptions) = LinterOptions.parse(warnOptions)
    val Right(errorLinterOptions) = LinterOptions.parse(errorOptions)

    // then
    warnLinterOptions.notificationLevel must be equalTo(Warn)
    errorLinterOptions.notificationLevel must be equalTo(Error)
  }

  @Test
  def enableOnlyTwoWarning() {
    // given
    val options = List("enable-only:UseExpm1+UseLog1p")

    // when
    val Right(linterOptions) = LinterOptions.parse(options)

    // then
    (Violation.AllNames.toSet -- linterOptions.disabledWarningNames).toList.sorted must be_==(List("UseExpm1", "UseLog1p"))
  }
}
