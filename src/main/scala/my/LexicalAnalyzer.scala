package my

import scala.collection.mutable.ArrayBuffer


object States extends Enumeration {
	type States = Value
	val H, CommentLine, SkipLine, IntNumber, DoubleNumber, Name, String, DefineSymbols, ExclamationSymbol, BoolOp = Value
}

// Stores key words
object LexemeType extends Enumeration {
	type LexemeType = Value
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

	val lexemesTable: LexemeTable = new LexemeTable

	def getState(): States.Value = state
	def getLineNumber(): Int = lineNumber

	/**
	 * Runs lexical analysis of the program given
	 * @param program string with the program to be compiled
	 */
	def run(program: String): Unit = {
		for (symbol <- program) {
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
		f"Line number ${lineNumber}"
	}

	/**
	 * Generates an error message to be printed inserting message given
	 * @param message message with a particular error
	 * @return error message
	 */
	private def getErrorMessage(message: String): String = {
		val errorPositionString = getSymbolPosition()
		errorHeader + message + "\n" + errorPositionString + "\n\n"
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
			case s if (LexemeType.constantLexemes(LexemeType.ArithmeticOp).contains(s.toString)) =>
				processArithmeticOpSymbol(symbol)
			case s if (LexemeType.constantLexemes(LexemeType.Brackets).contains(s.toString)) =>
				processBracketSymbol(symbol)
			case s if (s.isLetter | s == "_") => processFirstNameSymbol(symbol)
			case ' ' | '\t' =>
			case _ => System.err.println(getErrorMessage(f"Unable to parse symbol ${symbol}"))
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
			lexemesTable.add(new Lexeme(strBuffer, LexemeType.String, lineNumber))
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
			lexemesTable.add(new Lexeme(boolOpBuffer, LexemeType.BoolOp, lineNumber))
			boolOpBuffer = ""
			processSymbol(symbol)
		}

		def finishWithEqSign(): Unit = {
			lexemesTable.add(new Lexeme(boolOpBuffer + "=", LexemeType.BoolOp, lineNumber))
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
		def processError(): Unit = {
			System.err.println(getErrorMessage(f"Unable to parse lexeme: '!'"))
			state = States.H
			processSymbol(symbol)
		}

		state = States.H
		symbol match {
			case '=' => lexemesTable.add(new Lexeme("!=", LexemeType.BoolOp, lineNumber))
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
		nDefineSymbols match {
			case 1 => lexemesTable.add(new Lexeme("=", LexemeType.DefineOp, lineNumber))
			case 2 => lexemesTable.add(new Lexeme("==", LexemeType.BoolOp, lineNumber))
			case _ => System.err.println(getErrorMessage(f"Unable to parse lexeme: ${"="*nDefineSymbols}"))
		}

		state = States.H
		nDefineSymbols = 0
		processSymbol(symbol)
	}

	/**
	 * adds ':' to lexemeTable
	 */
	private def processColonSymbol(): Unit = {
		lexemesTable.add(new Lexeme(":", LexemeType.Colon, lineNumber))
	}

	/**
	 * adds ',' to lexemeTable
	 */
	private def processCommaSymbol(): Unit = {
		lexemesTable.add(new Lexeme(",", LexemeType.Comma, lineNumber))
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
	 * Decides what is the type of the name and adds it to lexemeTable
	 * @param symbol next symbol of the program
	 */
	private def finishName(symbol: Char): Unit = {
		val typeOptions = LexemeType.constantLexemes.filter(pair => {
			LexemeType.constantLexemes(pair._1).contains(nameBuffer)
		}).toArray
		val len = typeOptions.length
		len match {
			case 0 => lexemesTable.add(new Lexeme(nameBuffer, LexemeType.Name, lineNumber))
			case 1 => lexemesTable.add(new Lexeme(nameBuffer, typeOptions(0)._1, lineNumber))
			case _ => System.err.println(getErrorMessage(f"Unable to define lexeme type: ${nameBuffer}"))
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
		lexemesTable.add(new Lexeme(finalNumber, lineNumber))
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
		lexemesTable.add(new Lexeme(intNumberBuffer, lineNumber))
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
	}

	/**
	 * Symbol with arithmetic operation is added to the lexeme table
	 * @param symbol +, -, *, /, %
	 */
	private def processArithmeticOpSymbol(symbol: Char) = {
		lexemesTable.add(new Lexeme(symbol.toString, LexemeType.ArithmeticOp, lineNumber))
		previousState = state
		state = States.H
	}

	/**
	 * Bracket symbol is added to the lexeme table
	 * @param symbol [,],(,)
	 */
	private def processBracketSymbol(symbol: Char) = {
		lexemesTable.add(new Lexeme(symbol.toString, LexemeType.Brackets, lineNumber))
		previousState = state
		state = States.H
	}

}

class Lexeme(val value: String, val lexemeType: LexemeType.Value, val lineNumber: Int) {
	def this(intNumber: Int, lineNumber: Int) {
		this(intNumber.toString, LexemeType.IntNumber, lineNumber)
	}
	def this(doubleNumber: Double, lineNumber: Int) {
		this(doubleNumber.toString, LexemeType.DoubleNumber, lineNumber)
	}
	override def toString: String = f"${value} (${lineNumber}): ${lexemeType.toString}"
}

class LexemeTable {
	private val table: ArrayBuffer[Lexeme] = ArrayBuffer()
	private var n = 0  // table size
	private var idxLexeme = 0
	private var currentLexeme: Option[Lexeme] = None

	def current(): Option[Lexeme] = currentLexeme
	def size(): Int = n

	/**
	 * Prints table
	 */
	def print(): Unit = {
		table.foreach(lexeme => println(lexeme))
	}

	/**
	 * Appends new lexeme to the table
	 * @param lexeme new lexeme to be added
	 * @return this
	 */
	def add(lexeme: Lexeme): LexemeTable = {
		table.append(lexeme)
		n += 1
		this
	}

	/**
	 * @return next lexeme in table
	 */
	def next(): Option[Lexeme] = {
		if (idxLexeme >= n) {
			None
		} else {
			currentLexeme = Option(table(idxLexeme))
			idxLexeme += 1
			Option(currentLexeme.get)
		}
	}

	/**
	 * Resets current lexeme and index to start from the beginning
	 * @return this
	 */
	def restart(): LexemeTable = {
		idxLexeme = 0
		currentLexeme = None
		this
	}
}
