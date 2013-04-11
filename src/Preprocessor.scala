object Preprocessor {

    val BlockStartToken = "{"
    val BlockEndToken = "}"

    val TabSize = 4 //how many spaces does a tab take

    def preProcess(text: String): String = {
        val lines = text.split('\n').toList.filterNot(_.forall(isWhiteChar))
        val processedLines = BlockStartToken :: insertTokens(lines, List(0))
        processedLines.mkString("\n")
    }

    def insertTokens(lines: List[String], stack: List[Int]): List[String] = lines match {
        case List() => List.fill(stack.length) { BlockEndToken } //closing all opened blocks
        case line :: rest => {
            (computeIndentation(line), stack) match {
                case (indentation, top :: stackRest) if indentation > top => {
                    BlockStartToken :: line :: insertTokens(rest,  indentation :: stack)
                }
                case (indentation, top :: stackRest) if indentation == top =>
                    line :: insertTokens(rest, stack)
                case (indentation, top :: stackRest) if indentation < top => {
                    BlockEndToken :: insertTokens(lines, stackRest)
                }
                case _ => throw new IllegalStateException("Invalid algorithm")
            }
        }
    }


    private def computeIndentation(line: String): Int = {
        val whiteSpace = line takeWhile isWhiteChar
        (whiteSpace map {
            case ' ' => 1
            case '\t' => TabSize
        }).sum
    }

    private def isWhiteChar(ch: Char) = ch == ' ' || ch == '\t'
}