package my

import scala.collection.mutable.ArrayBuffer


object States extends Enumeration {
	type States = Value
	val H, CommentLine, SkipLine, IntNumber, DoubleNumber, String, ArithmeticSign = Value
}

object lexemeType extends Enumeration {
	type lexemeType = Value
	val ArithmeticOp, BoolOp, BoolVal, DoubleNumber, LangFunction, IntNumber, Type, KeyWord, Identifier = Value
	val keyWords = Map(
		ArithmeticOp -> Array("+", "-", "*", "/", "%"),
		BoolOp -> Array("and", "or"),
		BoolVal -> Array("True", "False"),
		LangFunction -> Array("read", "print", "println"),
		Type -> Array("Array", "Bool", "Double", "Int", "None", "String"),
		KeyWord -> Array("def", "for", "from", "if", "of", "return", "step", "to", "while")
	)
	def getlexemeTableItem(lexeme: String): Option[Value] = {
		val lexemeType = keyWords.filter(pair => pair._2.contains(lexeme)).toArray
		if (lexemeType.length == 1) {
			Option(lexemeType(0)._1)
		} else {
			None
		}
	}
}

class LexicalAnalyzer {
	private var lineNumber = 1
	private var symbolNumber = 0

	private var nCommentLineSlashes = 0
	private var intNumberBuffer = 0
	private var doubleNumberBuffer = 0.0
	private var doubleNumberPow10 = 0.1

	private var state = States.H
	private var previousState = States.H // should be saved in case of comment

	private val errorHeader = "Lexical error:\n"

	val lexemesTable: ArrayBuffer[(String, lexemeType.Value)] = ArrayBuffer()

	def getState(): States.Value = state
	def getLineNumber(): Int = lineNumber
	def getSymbolNumber(): Int = symbolNumber

	def printLexemeTable(): Unit = {
		lexemesTable.foreach(pair => println(pair._1 + ": " + pair._2.toString))
	}

	/**
	 * Runs lexical analysis of the program given
	 * @param program string with the program to be compiled
	 */
	def run(program: String): Unit = {
		for (symbol <- program) {
			symbolNumber += 1
			processSymbol(symbol)
		}
		processSymbol(' ')  // to finalize
	}

	/**
	 * Analyses next symbol of the program according to the current state
	 * @param symbol next symbol of the program
	 */
	private def processSymbol(symbol: Char): Unit = {
		state match {
			case States.H => processHState(symbol)
			case States.CommentLine => processCommentLineState(symbol)
			case States.SkipLine => processSkipLineState(symbol)
			case States.IntNumber => processIntNumberState(symbol)
			case States.DoubleNumber => processDoubleNumberState(symbol)
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
			case s if (s.isDigit) => processFirstNumberSymbol(symbol)
			case s if (lexemeType.keyWords(lexemeType.ArithmeticOp).contains(s.toString)) =>
				processArithmeticOpSymbol(symbol)
			case _ =>
		}
	}

	/**
	 * Double number processing:
	 * concatenates new symbols to the end of decimal part
	 * @param symbol next symbol of the program
	 */
	private def processDoubleNumberState(symbol: Char): Unit = {
		def processNumber(symbol: Char): Unit = {
			val newNum = symbol.toString.toInt
			doubleNumberBuffer += newNum * doubleNumberPow10
			doubleNumberPow10 *= 0.1
		}

		symbol match {
			case s if (s.isDigit) => processNumber(symbol)
			case s => finishDoubleNumber(s)
		}
	}

	/**
	 * Adds a lexeme with current double number to a lexemes table
	 * @param symbol next symbol of the program
	 */
	private def finishDoubleNumber(symbol: Char): Unit = {
		val finalNumber = intNumberBuffer + doubleNumberBuffer
		lexemesTable.append((finalNumber.toString, lexemeType.DoubleNumber))
		intNumberBuffer = 0
		doubleNumberBuffer = 0
		doubleNumberPow10 = 0.1
		previousState = state
		state = States.H
		processSymbol(symbol)
	}

	/**
	 * Digit arrived => number has started.
	 * intNumberBuffer will accumulate the number
	 */
	private def processFirstNumberSymbol(symbol: Char): Unit = {
		state = States.IntNumber
		intNumberBuffer = symbol.toString.toInt
	}

	/**
	 * Int number processing:
	 * wait for a delimiter or for a '.' to move to DoubleNumber state
	 * @param symbol next symbol of the program
	 */
	private def processIntNumberState(symbol: Char): Unit = {
		def convertStateToDoubleNumber(): Unit = {
			state = States.DoubleNumber
		}

		symbol match {
			case s if (s.isDigit) => intNumberBuffer = intNumberBuffer * 10 + symbol.toString.toInt
			case '.' => convertStateToDoubleNumber()
			case s => finishIntNumber(s)
		}
	}

	/**
	 * Adds a lexeme with current number to a lexemes table
	 * @param symbol symbol that came after number
	 */
	private def finishIntNumber(symbol: Char): Unit = {
		lexemesTable.append((intNumberBuffer.toString, lexemeType.IntNumber))
		intNumberBuffer = 0
		previousState = state
		state = States.H
		processSymbol(symbol)
	}

	/**
	 * Sets initial state when slash has arrived
	 */
	private def processFirstCommentLineSlashSymbol(): Unit = {
		nCommentLineSlashes = 1
		previousState = state
		state = States.CommentLine
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

		def convertToDivisionOp(symbol: Char): Unit = {
			state = States.ArithmeticSign
			processArithmeticOpSymbol('/')
			state = States.H
			processSymbol(symbol)
		}

		symbol match {
			case '\n' if (nCommentLineSlashes >= 2) => processEndOfCommentLine()
			case '/' if (nCommentLineSlashes == 1) => nCommentLineSlashes = 2
			case _ if (nCommentLineSlashes < 2) => convertToDivisionOp(symbol)
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
	 * Updates lineNumber and symbolNumber values
	 * Should be called if '\n' arrived
	 */
	private def processNewLineSymbol(): Unit = {
		lineNumber += 1
		symbolNumber = 0
	}

	/**
	 * Symbol with arithmetic operation is added to the lexeme table
	 * @param symbol +, -, *, /, %
	 */
	private def processArithmeticOpSymbol(symbol: Char) = {
		lexemesTable.append((symbol.toString, lexemeType.ArithmeticOp))
		previousState = state
		state = States.H
	}

}