/**
 * Copyright 2012 Foursquare Labs, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.foursquare.lint

import annotation.tailrec

case class LinterOptions(notificationLevel: NotificationLevel = Warn, disabledWarningNames: Seq[String] = Nil)

sealed trait NotificationLevel

object Warn extends NotificationLevel

object Error extends NotificationLevel

object LinterOptions {
  def parse(options: List[String]): Either[String, LinterOptions] = parse0(options, new LinterOptions)

  final val EnableOnlyArgument = "enable-only"
  final val DisableArgument = "disable"
  final val CompilerLogLevel = "level"
  final val WarningNameDelimiter = "\\+"
  final val OptionKeyValueDelimiter = ":"

  private def parseWarningList(fullOption: String): Either[String, Seq[String]] = fullOption.split(OptionKeyValueDelimiter) match {
    case Array(option, warningNames) =>
      val (validNames, invalidNames) = warningNames.split(WarningNameDelimiter).partition(Violation.NameToWarning.contains)
      if (validNames.nonEmpty && invalidNames.isEmpty) Right(validNames)
      else Left("The '%s' option referenced invalid warnings: %s".format(option, invalidNames.mkString(", ")))
    case _ => Left("The '%s' option was not of the expected form.")
  }

  private def parseNotificationLevel(fullOption: String): Either[String, NotificationLevel] = fullOption.split(OptionKeyValueDelimiter) match {
    case Array(option, level) =>
      level match {
        case "warn" => Right(Warn)
        case "error" => Right(Error)
        case _ => Left(s"$fullOption is invalid. Only warn and error are supported")
      }
    case _ => Left(s"$fullOption was not of the expected form")
  }

  @tailrec
  private def parse0(options: List[String], linterOptions: LinterOptions): Either[String, LinterOptions] = options match {
    case Nil => Right(linterOptions)
    case option :: xs if (option.startsWith(CompilerLogLevel)) => parseNotificationLevel(option) match {
      case Right(level) => parse0(xs, linterOptions.copy(notificationLevel = level))
      case Left(error) => Left(error)
    }
    case option :: xs if (option.startsWith(DisableArgument) || option.startsWith(EnableOnlyArgument)) => parseWarningList(option) match {
      case Right(warnings) if option.startsWith(EnableOnlyArgument) => parse0(xs, linterOptions.copy(disabledWarningNames = Violation.AllNames.diff(warnings)))
      case Right(warnings) => parse0(xs, linterOptions.copy(disabledWarningNames = warnings))
      case Left(errorMessage) => Left(errorMessage)
    }
    case unknownOption :: _ => Left("The option '%s' is unrecognized.".format(unknownOption))
  }
}
