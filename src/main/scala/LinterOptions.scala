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

import scala.annotation.tailrec

final case class LinterOptions(disabledWarningNames: Seq[String] = Nil, printWarningNames: Boolean = true)

final object LinterOptions {
  def parse(options: List[String]): Either[String, LinterOptions] = parse0(options, new LinterOptions)

  val EnableOnlyArgument = "enable-only"
  val DisableArgument = "disable"
  val PrintWarningNames = "printWarningNames"
  val WarningNameDelimiter = "\\+"
  val OptionKeyValueDelimiter = ":"

  private[this] def parseWarningList(fullOption: String): Either[String, Seq[String]] = fullOption.split(OptionKeyValueDelimiter) match {
    case Array(option, warningNames) =>
      val (validNames, invalidNames) = warningNames.split(WarningNameDelimiter).partition(Warning.NameToWarning.contains)
      if (validNames.nonEmpty && invalidNames.isEmpty) Right(validNames)
      else Left(s"The '$option' option referenced invalid warnings: ${invalidNames.mkString(", ")}")
    case _ => Left(s"The '$fullOption' option was not of the expected form.")
  }

  @tailrec
  private[this] def parse0(options: List[String], linterOptions: LinterOptions): Either[String, LinterOptions] = options match {
    case Nil => Right(linterOptions)
    case option :: xs if (option.startsWith(DisableArgument) || option.startsWith(EnableOnlyArgument)) => parseWarningList(option) match {
      case Right(warnings) if option.startsWith(EnableOnlyArgument) => parse0(xs, linterOptions.copy(disabledWarningNames = Warning.AllNames.diff(warnings)))
      case Right(warnings) => parse0(xs, linterOptions.copy(disabledWarningNames = warnings))
      case Left(errorMessage) => Left(errorMessage)
    }
    case option :: _ if option.startsWith(PrintWarningNames) => option.split(OptionKeyValueDelimiter) match {
      case Array(_, value @ ("true" | "false")) => Right(linterOptions.copy(printWarningNames = value.toBoolean))
      case Array(_) if option == PrintWarningNames => Right(linterOptions.copy(printWarningNames = true))
      case _ => Left(s"The '$option' option was not of the expected form")
    }
    case unknownOption :: _ => Left(s"The option '$unknownOption' is unrecognized.")
  }
}
