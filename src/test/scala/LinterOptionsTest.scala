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

import org.junit.Test
import org.specs2.matcher.MustThrownMatchers

class LinterOptionsTest extends MustThrownMatchers {
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
