package my


object States extends Enumeration {
	type States = Value
	val H, CommentLine, SkipLine = Value
}

class LexicalAnalyzer {
	private var lineNumber = 1
	private var symbolNumber = 0

	private var nCommentLineSlashes = 0

	private var state = States.H
	private var previousState = States.H // should be saved in case of comment

	private val errorHeader = "Lexical error:\n"

	def getState(): States.Value = state
	def getLineNumber(): Int = lineNumber
	def getSymbolNumber(): Int = symbolNumber

	/**
	 * Runs lexical analysis of the program given
	 * @param program string with the program to be compiled
	 */
	def run(program: String): Unit = {
		for (symbol <- program) {
			processSymbol(symbol)
		}
	}

	/**
	 * Analyses next symbol of the program according to the current state
	 * @param symbol next symbol of the program
	 */
	private def processSymbol(symbol: Char): Unit = {
		symbolNumber += 1
		state match {
			case States.H => processHState(symbol)
			case States.CommentLine => processCommentLineState(symbol)
			case States.SkipLine => processSkipLineState(symbol)
		}
	}

	/**
	 * Generates a string with current symbol position
	 * (line and symbol in line)
	 * @return string with current symbol position
	 */
	private def getSymbolPosition(): String = {
		"Line number " + lineNumber + "; symbol number " + symbolNumber
	}

	/**
	 * Initial state processing
	 * @param symbol next symbol of the program
	 */
	private def processHState(symbol: Char): Unit = {
		symbol match {
			case '\n' => processNewLineSymbol()
			case '/' => processFirstCommentLineSlashSymbol()
			case _ =>
		}
	}

	/**
	 * Processes of the comment that starts from //:
	 * skips current line and print an error message in case there is only one slash
	 * @param symbol next symbol of the program
	 */
	private def processCommentLineState(symbol: Char): Unit = {
		/**
		 * Generates an error message to be printed
		 * in case one slash in comment line start is missing
		 * @return error message
		 */
		def getCommentErrorMessage(): String = {
			val errorPositionString = getSymbolPosition()
			val errorMessage = errorHeader + "Invalid comment start: only one '/' found\n"
			errorMessage + errorPositionString + "\n\n"
		}

		/**
		 * Comment slash is missing => print message and skip current line
		 */
		def processCommentLineError(): Unit = {
			state = States.SkipLine
			nCommentLineSlashes = 0
			System.err.println(getCommentErrorMessage())
		}

		symbol match {
			case '\n' if (nCommentLineSlashes >= 2) => processEndOfCommentLine()
			case '/' if (nCommentLineSlashes == 1) => nCommentLineSlashes = 2
			case _ if (nCommentLineSlashes < 2) => processCommentLineError()
			case _ =>
		}
	}

	/**
	 * Ignores all symbols until '\n' arrives
	 * @param symbol next symbol of the program
	 */
	private def processSkipLineState(symbol: Char): Unit = {
		symbol match {
			case '\n' => state = previousState
			case _ =>
		}
	}

	/**
	 * Comment has finished
	 */
	private def processEndOfCommentLine(): Unit = {
		nCommentLineSlashes = 0
		state = previousState
		processNewLineSymbol()
	}

	/**
	 * Updates lineNumber and symbolNumber values
	 * Should be called if '\n' arrived
	 */
	private def processNewLineSymbol(): Unit = {
		lineNumber += 1
		symbolNumber = 0
	}

	/**
	 * Sets initial state when slash has arrived
	 */
	private def processFirstCommentLineSlashSymbol(): Unit = {
		nCommentLineSlashes = 1
		previousState = state
		state = States.CommentLine
	}


}