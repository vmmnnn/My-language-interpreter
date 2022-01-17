package my

import scala.collection.mutable.ArrayBuffer


object States extends Enumeration {
	type States = Value
	val H, CommentLine, SkipLine, IntNumber, DoubleNumber, Name, String, DefineSymbols, ExclamationSymbol, BoolOp = Value
}

// Stores key words
object lexemeType extends Enumeration {
	type lexemeType = Value
	val ArithmeticOp, BoolOp, BoolVal, Brackets, Colon, Comma, DefineOp, DoubleNumber, IntNumber, KeyWord, LangFunction, Name, String, Type = Value
	val constantLexemes = Map(
		ArithmeticOp -> Array("+", "-", "*", "/", "%"),
		BoolOp -> Array("and", "or", "not", "==", "!=", ">", ">=", "<", "<="),
		BoolVal -> Array("True", "False"),
		Brackets -> Array("(", ")", "[", "]", "{", "}"),
		DefineOp -> Array("="),
		Colon -> Array(":"),
		Comma -> Array(","),
		KeyWord -> Array("def", "for", "from", "if", "else", "of", "return", "step", "to", "while"),
		LangFunction -> Array("read", "print", "println"),
		Type -> Array("Array", "Bool", "Double", "Int", "None", "String")
	)
}

/*
	Finite-state machine
	Use run-function and get result in lexemesTable variable
 */
class LexicalAnalyzer {
	private var lineNumber = 1
	private var symbolNumber = 0

	private var nCommentLineSlashes = 0
	private var intNumberBuffer = 0
	private var doubleNumberBuffer = 0.0
	private var doubleNumberPow10 = 0.1
	private var nameBuffer = ""
	private var nDefineSymbols = 0
	private var boolOpBuffer = ""
	private var strBuffer = ""

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
			case States.Name => processNameState(symbol)
			case States.DefineSymbols => processDefineState(symbol)
			case States.ExclamationSymbol => processExclamationState(symbol)
			case States.BoolOp => processBoolOpState(symbol)
			case States.String => processStringState(symbol)
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
			case ':' => processColonSymbol()
			case ',' => processCommaSymbol()
			case '=' => processFirstDefineSymbol()
			case '!' => processExclamationSymbol()
			case '<' | '>' => processBoolOpSymbol(symbol)
			case '"' => processFirstStringSymbol()
			case s if (s.isDigit) => processFirstNumberSymbol(symbol)
			case s if (lexemeType.constantLexemes(lexemeType.ArithmeticOp).contains(s.toString)) =>
				processArithmeticOpSymbol(symbol)
			case s if (lexemeType.constantLexemes(lexemeType.Brackets).contains(s.toString)) =>
				processBracketSymbol(symbol)
			case s if (s.isLetter | s == "_") => processFirstNameSymbol(symbol)
			case _ =>
		}
	}

	/**
	 * Sets state to String
	 */
	private def processFirstStringSymbol(): Unit = {
		state = States.String
	}

	/**
	 *
	 * @param symbol next symbol of the program
	 */
	private def processStringState(symbol: Char): Unit = {
		def finish(): Unit = {
			state = States.H
			lexemesTable.append((strBuffer, lexemeType.String))
			strBuffer = ""
		}

		symbol match {
			case '"' => finish()
			case s => strBuffer = strBuffer + s.toString
		}
	}

	/**
	 * Checks if we have <, > or <=, >=
	 * @param symbol > or <
	 */
	private def processBoolOpSymbol(symbol: Char): Unit = {
		state = States.BoolOp
		boolOpBuffer = symbol.toString
	}

	/**
	 * Finishes >, < or >=, <=
	 * @param symbol next symbol of the program
	 */
	private def processBoolOpState(symbol: Char): Unit = {
		def finishNoEqSign(symbol: Char): Unit = {
			lexemesTable.append((boolOpBuffer, lexemeType.BoolOp))
			boolOpBuffer = ""
			processSymbol(symbol)
		}

		def finishWithEqSign(): Unit = {
			lexemesTable.append((boolOpBuffer + "=", lexemeType.BoolOp))
			boolOpBuffer = ""
		}

		state = States.H
		symbol match {
			case '=' => finishWithEqSign()
			case s => finishNoEqSign(s)
		}
	}

	/**
	 * Set ExclamationSymbol state
	 */
	private def processExclamationSymbol(): Unit = {
		state = States.ExclamationSymbol
	}

	/**
	 * Checks if there is !=
	 * Otherwise - error
	 * @param symbol next symbol of the program
	 */
	private def processExclamationState(symbol: Char): Unit = {
		def getCommentErrorMessage(): String = {
			val errorPositionString = getSymbolPosition()
			val errorMessage = f"$errorHeader Unable to parse lexeme: '!'\n"
			errorMessage + errorPositionString + "\n\n"
		}

		def processError(): Unit = {
			System.err.println(getCommentErrorMessage())
			processSymbol(symbol)
		}

		state = States.H
		symbol match {
			case '=' => lexemesTable.append(("!=", lexemeType.BoolOp))
			case _ => processError()
		}
	}

	/**
	 * Starts accumulating '=' symbols
	 */
	private def processFirstDefineSymbol(): Unit = {
		state = States.DefineSymbols
		nDefineSymbols += 1
	}

	/**
	 * Keeps accumulating '=' symbols until they come
	 * Otherwise get the type of a sequence (BoolOp or DefineOp)
	 * @param symbol next symbol of the program
	 */
	private def processDefineState(symbol: Char): Unit = {
		symbol match {
			case '=' => nDefineSymbols += 1
			case s => finishDefineState(s)
		}
	}

	/**
	 * Determines the type of the '=' sequence:
	 * BoolOp or DefineOp for '==' and '=' respectively
	 * @param symbol
	 */
	private def finishDefineState(symbol: Char): Unit = {
		def getCommentErrorMessage(): String = {
			val errorPositionString = getSymbolPosition()
			val errorMessage = f"$errorHeader Unable to parse lexeme: ${"="*nDefineSymbols}\n"
			errorMessage + errorPositionString + "\n\n"
		}

		nDefineSymbols match {
			case 1 => lexemesTable.append(("=", lexemeType.DefineOp))
			case 2 => lexemesTable.append(("==", lexemeType.BoolOp))
			case _ => System.err.println(getCommentErrorMessage())
		}

		state = States.H
		nDefineSymbols = 0
		processSymbol(symbol)
	}

	/**
	 * Appends ':' to lexemeTable
	 */
	private def processColonSymbol(): Unit = {
		lexemesTable.append((":", lexemeType.Colon))
	}

	/**
	 * Appends ',' to lexemeTable
	 */
	private def processCommaSymbol(): Unit = {
		lexemesTable.append((",", lexemeType.Comma))
	}

	/**
	 * Saves symbol in nameBuffer
	 * @param symbol first symbol of the sequence
	 */
	private def processFirstNameSymbol(symbol: Char): Unit = {
		state = States.Name
		nameBuffer = symbol.toString
	}

	/**
	 * Accumulates symbols in nameBuffer until they can be part of name
	 * @param symbol next symbol of the program
	 */
	private def processNameState(symbol: Char): Unit = {
		symbol match {
			case s if (s.isDigit | s.isLetter | s == '_') => nameBuffer = nameBuffer + symbol.toString
			case s => finishName(s)
		}
	}

	/**
	 * Decides what is the type of the name and appends it to lexemeTable
	 * @param symbol next symbol of the program
	 */
	private def finishName(symbol: Char): Unit = {
		def getCommentErrorMessage(): String = {
			val errorPositionString = getSymbolPosition()
			val errorMessage = f"$errorHeader Unable to define lexeme type: ${nameBuffer}\n"
			errorMessage + errorPositionString + "\n\n"
		}

		val typeOptions = lexemeType.constantLexemes.filter(pair => {
			lexemeType.constantLexemes(pair._1).contains(nameBuffer)
		}).toArray
		val len = typeOptions.length
		len match {
			case 0 => lexemesTable.append((nameBuffer, lexemeType.Name))
			case 1 => lexemesTable.append((nameBuffer, typeOptions(0)._1))
			case _ => System.err.println(getCommentErrorMessage())
		}
		state = States.H
		nameBuffer = ""
		processSymbol(symbol)
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
			processArithmeticOpSymbol('/')
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

	/**
	 * Bracket symbol is added to the lexeme table
	 * @param symbol [,],(,)
	 */
	private def processBracketSymbol(symbol: Char) = {
		lexemesTable.append((symbol.toString, lexemeType.Brackets))
		previousState = state
		state = States.H
	}

}