package TAPY

import scala.util.parsing.combinator.RegexParsers

object PYParser extends RegexParsers {
	lazy val AugAssignKeywords = "+=" | "-=" | "*=" | "/=" | "%=" | "&=" | "|=" | "^=" | "<<=" | ">>=" | "**=" | "//="
}