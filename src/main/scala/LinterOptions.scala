package com.foursquare.lint

case class LinterOptions(disabledWarningNames: Seq[String] = Nil)

object LinterOptions {
  def parse(options: List[String]): Either[String, LinterOptions] = parse0(options, new LinterOptions)

  final val EnableOnlyArgument = "enable-only"
  final val DisableOnlyArgument = "disable-only"
  final val WarningNameDelimiter = ","
  final val OptionKeyValueDelimiter = ":"

  private def parseWarningList(fullOption: String): Either[String, Seq[String]] = fullOption.split(OptionKeyValueDelimiter) match {
    case Array(option, warningNames) =>  
      val (validNames, invalidNames) = warningNames.split(WarningNameDelimiter).partition(Warning.NameToWarning.contains) 
      if (validNames.nonEmpty && invalidNames.isEmpty) Right(validNames)
      else Left("The '%s' option referenced invalid warnings: %s".format(option, invalidNames.mkString(", ")))
    case _ => Left("The '%s' option was not of the expected form.")
  }
    
  @annotation.tailrec
  private def parse0(options: List[String], linterOptions: LinterOptions): Either[String, LinterOptions] = options match {
    case Nil => Right(linterOptions)
    case option :: xs if (option.startsWith(DisableOnlyArgument) || option.startsWith(EnableOnlyArgument)) => parseWarningList(option) match {
      case Right(warnings) if option.startsWith(DisableOnlyArgument) => parse0(xs, linterOptions.copy(disabledWarningNames = warnings))
      case Right(warnings) => parse0(xs, linterOptions.copy(disabledWarningNames = Warning.AllNames.diff(warnings).toSeq)) 
      case Left(errorMessage) => Left(errorMessage)
    }
    case unknownOption :: _ => Left("The option '%s' is unrecognized.".format(unknownOption))
  }
}
