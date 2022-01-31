package my

import my.LexemeType.{ArithmeticOp, BoolOp, BoolVal, Brackets, Colon, Comma, DoubleNumber, IntNumber, Name, Semicolon, Type}

import scala.collection.mutable.Map


object VarType extends Enumeration {
	type VarType = Value
	val Array, Bool, Double, Int, String, None = Value
	def getType(str: String): VarType.Value = {
		str match {
			case "Array" => Array
			case "Bool" => Bool
			case "Double" => Double
			case "Int" => Int
			case "String" => String
			case "None" => None
		}
	}
}

/**
 * Value of the variable in variables table
 * @param varType variable type
 * @param value variable value in a string format
 */
class Value(val varType: VarType.Value, var value: Option[String]) {
	def isSame(otherValue: Value): Boolean = {
		if (value != otherValue.value) return false
		if (varType != otherValue.varType) return false
		true
	}

	override def toString: String = {
		if (value.isEmpty) f"$varType"
		else f"${varType}: ${value.get}"
	}
}

/**
 * Class for variables table
 */
class VarTable {
	private val table: Map[String, Value] = Map.empty
	def getTable: Map[String, Value] = table

	def getVal(name: String): Option[Value] = table.get(name)

	def setVal(name: String, value: Value): Unit = {
		table(name) = value
	}

	def print(): Unit = {
		table.foreach{case (_, variable) => println(variable)}
	}
}

/**
 * Parses lexemes given, fills function and globalVars tables (parse function) and runs the program (run function)
 * @param lexemeTable result of lexical analysis
 */
class LexemesParser(lexemeTable: LexemeTable) {
	// current lexeme, updates during parsing
	private var lexeme = lexemeTable.next()

	// table with global variables
	private val globalVars = new VarTable

	// table of functions: name, 'def'-lexeme position in LexemeTable
	private val functionTable: Map[String, Int] = Map.empty

	private val priorityOp: Map[String, Int] = Map(
		"not" -> 0,
		"*" -> 1, "/" -> 1, "%" -> 1,
		"+" -> 2, "-" -> 2,
		">" -> 3, ">=" -> 3, "<" -> 3, "<=" -> 3,
		"==" -> 4, "!=" -> 4,
		"and" -> 5, "or" -> 5
	)

	def getGlobalVars: VarTable = globalVars
	def getFunctionTable: Map[String, Int] = functionTable

	/**
	 * Starts parsing
	 */
	def parse(): LexemesParser = {
		// program starts either from global variables or right from functions
		while (lexeme.isDefined) {
			if (lexeme.get.lexemeType == LexemeType.Name) { // global variables
				val (name, value) = getVariable()
				globalVars.setVal(name, value)
			} else if (lexeme.get.value == "def") { // function
				parseFunction()
			} else { // something else => error
				sendUnexpectedTokenError()
			}
		}
		this
	}

	/**
	 * Start interpreting. MainFunction will be called<br>
	 * Main function should have no parameters and return None
	 * @param mainFunction function to be called, usually 'main'
	 */
	def run(mainFunction: String = "main"): Unit = {
		val mainFunctionLexemeIdx = functionTable.get(mainFunction)
		if (mainFunctionLexemeIdx.isEmpty) {
			sendError(f"No function with name $mainFunction is provided", lexemeTable.size)
		}
		lexemeTable.setLexemeIdx(mainFunctionLexemeIdx.get)
		lexeme = lexemeTable.current
		checkMainHeader()
		nextLexemeCheckEmpty()
		while (lexeme.get.value != "}") {
			runBlock()
		}
	}

	/**
	 * Executes next command or command block (if, for etc)<br>
	 * Should be called when current lexeme is the block beginning
	 */
	private[my] def runBlock(): Unit = {
		lexeme.get.value match {
			case "print" => runPrint(false)
			case "println" => runPrint(true)
		}
	}

	/**
	 * Runs model language function 'print' or 'println' with newLine is false or true respectively
	 * @param newLine flag that shows if '\n' should be printed at the end
	 */
	private[my] def runPrint(newLine: Boolean): Unit = {
		checkNextLexemeValue("(")
		nextLexemeCheckEmpty()
		val printValue = compute()
	}

	/**
	 * Computes arithmetic or boolean expression that starts from current lexeme<br>
	 * Works with Reverse Polish notation
	 * @return Value: expression type and its result in a string format
	 */
	private[my] def compute(): Value = {
		// to add brackets and operations as RPN suggests
		var stackOp: List[Lexeme] =
			List(new Lexeme("(", LexemeType.Brackets, lexeme.get.lineNumber))
		var RPN: List[Lexeme] = List.empty
		var stackCompute: List[Lexeme] = List.empty

		def moveToRPN(): Unit = {
			var stackOpHead = stackOp.head
			while (stackOpHead.value != "(") {
				stackOp = stackOp.tail
				RPN = RPN :+ stackOpHead
				stackOpHead = stackOp.head
			}
			stackOp = stackOp.tail
			nextLexemeCheckEmpty()
		}

		def processOp(): Unit = {
			var stackOpHead = stackOp.head
			if (stackOpHead.value == "(") {
				stackOp = lexeme.get :: stackOp
			} else if (priorityOp(stackOpHead.value) > priorityOp(lexeme.get.value)) {
				stackOp = lexeme.get :: stackOp
			} else {
				var lessPriority = true
				while (lessPriority) {
					stackOp = stackOp.tail
					RPN = RPN :+ stackOpHead
					stackOpHead = stackOp.head
					if (!Set(ArithmeticOp, BoolOp).contains(stackOpHead.lexemeType)) {
						lessPriority = false
					} else if (priorityOp(stackOpHead.value) > priorityOp(lexeme.get.value)) {
						lessPriority = false
					}
				}
				stackOp = lexeme.get :: stackOp
			}
			nextLexemeCheckEmpty()
		}

		def addToRPN(): Unit = {
			RPN = RPN :+ lexeme.get
			nextLexemeCheckEmpty()
		}

		def addToStackOp(): Unit = {
			stackOp = lexeme.get :: stackOp
			nextLexemeCheckEmpty()
		}

		def createRPN(): Unit = {
			val expressionLineNumber = lexeme.get.lineNumber
			while (lexeme.get.lexemeType != Semicolon) {
				lexeme.get.lexemeType match {
					case IntNumber | DoubleNumber | BoolVal => addToRPN()
					case Name if (globalVars.getVal(lexeme.get.value).isDefined) => addToRPN()
					case Brackets if (lexeme.get.value == "(") => addToStackOp()
					case Brackets if (lexeme.get.value == ")") => moveToRPN()
					case BoolOp | ArithmeticOp => processOp()
				}
			}
			var stackOpHead = stackOp.head
			while (stackOpHead.value != "(") {
				RPN = RPN :+ stackOpHead
				stackOp = stackOp.tail
				stackOpHead = stackOp.head
			}
			if (stackOp.length != 1) {
				sendError("Expression cannot be computed: parenthesis is not closed", expressionLineNumber)
			}
		}

		def evaluate(): Value = {
			new Value(VarType.Int, Option("24"))  // TODO write the function
		}

		createRPN()
		evaluate()
	}


	/**
	 * Check if main function has no parameters and returns None<br>
	 * Should be called when current lexeme is
	 */
	private[my] def checkMainHeader(): Unit = {
		checkNextLexemeValue("(")
		nextLexemeCheckEmpty()
		if (lexeme.get.value != ")") {
			sendExpectedFoundError(f"Main function should not have parameters: )")
		}
		checkNextLexemeType(Colon, "Main function type (None)")
		nextLexemeCheckEmpty()
		if (lexeme.get.value != "None") {
			sendExpectedFoundError(f"Main function type (None)")
		}
		checkNextLexemeValue("{")
	}

	/**
	 * Sends a message to stderr and throws an Exception
	 * @param message message to be printed
	 */
	private[my] def sendError(message: String, lineNumber: Int): Unit = {
		System.err.println(f"line ${lineNumber}: $message")
		throw new Exception
	}

	/**
	 * Sends "Unexpected token '...'" message with current lexeme
	 */
	private[my] def sendUnexpectedTokenError(): Unit = {
		sendError(f"Unexpected token ${lexeme.get.value}", lexeme.get.lineNumber)
	}

	/**
	 * Sends "Unexpected token '...'" message with token and position given
	 * @param token wrong lexeme found
	 * @param lineNumber position
	 */
	private[my] def sendUnexpectedTokenError(token: String, lineNumber: Int): Unit = {
		sendError(f"Unexpected token $token", lineNumber)
	}

	/**
	 * Sends "'...' expected, but '...' found" message
	 * @param expected what was expected
	 */
	private[my] def sendExpectedFoundError(expected: String): Unit = {
		sendError(f"$expected expected, but ${lexeme.get.value} found", lexeme.get.lineNumber)
	}

	/**
	 * Takes next lexeme from lexeme table when it has to exist
	 */
	private[my] def nextLexemeCheckEmpty(): Unit = {
		val lineNumber = lexeme.get.lineNumber
		lexeme = lexemeTable.next()
		if (lexeme.isEmpty) {
			sendError("Unexpected end of program", lineNumber)
		}
	}

	/**
	 * Should be called when current lexeme is ':' after Name
	 * @return variable type according to lexeme
	 */
	private[my] def getVarType(): VarType.Value = {
		nextLexemeCheckEmpty()
		if (lexeme.get.lexemeType != LexemeType.Type) {
			val lineNumber = lexeme.get.lineNumber
			sendError("Variable type expected after ':'", lineNumber)
		}
		VarType.getType(lexeme.get.value)
	}

	/**
	 * Checks if lexeme is Int, Double, Bool or String value
	 * @param lexeme lexeme to be checked
	 * @return true/false
	 */
	private[my] def isValue(lexeme: Lexeme): Boolean = {
		lexeme.lexemeType match {
			case BoolVal => true
			case DoubleNumber => true
			case IntNumber => true
			case LexemeType.String => true
			case _ => false
		}
	}

	/**
	 * Parse lexemes starting from the current position
	 * and gets variable<br>
	 * Should be called if current lexeme is a Name
	 * @return pair (variable name, its value) to be added to variables table
	 */
	private[my] def getVariable(): (String, Value) = {
		val varName = lexeme.get.value
		var varType: Option[VarType.Value] = None
		var varValue: Option[String] = None

		def getValueCheckType(): Unit = {
			varValue = Option(lexeme.get.value)
			lexeme.get.lexemeType match {
				case BoolVal if varType.get == VarType.Bool =>
				case DoubleNumber if varType.get == VarType.Double =>
				case IntNumber if varType.get == VarType.Int =>
				case IntNumber if varType.get == VarType.Double =>
				case LexemeType.String if varType.get == VarType.String =>
				case _ => sendExpectedFoundError(f"Types do not match: ${varType.get}")
			}
		}

		def getValueSetType(): Unit = {
			if (!isValue(lexeme.get)) {
				sendUnexpectedTokenError()
			}
			varValue = Option(lexeme.get.value)
			varType = Option(lexeme.get.lexemeType match {
				case BoolVal => VarType.Bool
				case DoubleNumber => VarType.Double
				case IntNumber => VarType.Int
				case LexemeType.String => VarType.String
			})
		}

		nextLexemeCheckEmpty()

		// Colon after variable name => look for a type
		var lineNumber = 0
		if (lexeme.get.lexemeType == LexemeType.Colon) {
			varType = Option(getVarType())
			lineNumber = lexeme.get.lineNumber
			lexeme = lexemeTable.next()
		}

		if (lexeme.isEmpty) {
			if (varType.isEmpty) {
				sendUnexpectedTokenError(varName, lineNumber)
			}
			return (varName, new Value(varType.get, None))
		}

		// Neither colon for type nor '=' for value were found
		if (varType.isEmpty & (lexeme.get.lexemeType != LexemeType.DefineOp)) {
			sendUnexpectedTokenError(varName, lineNumber)
		}

		// '=' => look for a value
		if (lexeme.get.lexemeType == LexemeType.DefineOp) {   // TODO: value is a boolean or arithmetic expression
			nextLexemeCheckEmpty()
			if (varType.isEmpty) {  // define type according to the value
				getValueSetType()
			} else {  // type of the value and the declared one must be the same
				getValueCheckType()
			}
		}

		lexeme = lexemeTable.next()
		(varName, new Value(varType.get, varValue))
	}

	/**
	 * Called when 'def' key word found<br>
	 * Saves functions names with its idx in lexemeTable in function table to call it if needed
	 */
	private[my] def parseFunction(): Unit = {
		checkNextLexemeType(Name, "Function name")
		val name = lexeme.get.value
		val checkIfExists = functionTable.get(name)
		if (checkIfExists.isDefined) {  // the function has been already defined
			val errorMessage = f"Duplicated function name $name."
			val previousDefinitionMessage = f"Previous declaration in line $checkIfExists"
			sendError(f"$errorMessage\n$previousDefinitionMessage", lexeme.get.lineNumber)
		}
		functionTable(name) = lexemeTable.getLexemeIdx
		skipFunction()
	}

	/**
	 * Gets next lexeme and check its type
	 * @param expected expected type
	 * @param expectedErrorMessage what was expected (to be printed in expected-found error)
	 */
	private[my] def checkNextLexemeType(expected: LexemeType.Value, expectedErrorMessage: String): Unit = {
		nextLexemeCheckEmpty()
		if (lexeme.get.lexemeType != expected) {
			sendExpectedFoundError(expectedErrorMessage)
		}
	}

	/**
	 * Gets next lexeme and check if we got token that was expected
	 * @param expected expected token
	 */
	private[my] def checkNextLexemeValue(expected: String): Unit = {
		nextLexemeCheckEmpty()
		if (lexeme.get.value != expected) {
			sendExpectedFoundError(f"'$expected'")
		}
	}

	/**
	 * Skips lexemes until function ends (def name(...): Type {...})<br>
	 * Should be called when lexeme is a function name
 	 */
	private[my] def skipFunction(): Unit = {
		def functionParameters(): Unit = {
			while (lexeme.get.lexemeType == Name) {
				checkNextLexemeType(Colon, "Type")
				checkNextLexemeType(Type, "Type")

				nextLexemeCheckEmpty() // either ',' or ')'
				if (lexeme.get.lexemeType == Comma) {
					nextLexemeCheckEmpty()
					if (lexeme.get.lexemeType != Name) {
						sendExpectedFoundError("Next parameter after ','")
					}
				}
			}
		}

		// def name(...): Type {
		def header(): Unit = {
			nextLexemeCheckEmpty()
			if (lexeme.get.lexemeType != Brackets | lexeme.get.value != "(") {
				sendExpectedFoundError("Function parameters in brackets")
			}
			nextLexemeCheckEmpty()
			functionParameters()
			if (lexeme.get.value != ")") {
				sendUnexpectedTokenError()
			}
			checkNextLexemeType(Colon, "Function type")
			checkNextLexemeType(Type, "Function type")
			checkNextLexemeValue("{")
		}

		def isPairedBrackets(open: String, close: String): Boolean = {
			open match {
				case "(" if close == ")" => true
				case "{" if close == "}" => true
				case "[" if close == "]" => true
				case _ => false
			}
		}

		def bracketsBalance(): Unit = {
			var bracketsStack: List[String] = List.empty
			val openBrackets = Array("(", "{", "[")
			val closeBrackets = Array(")", "}", "]")
			bracketsStack = "{" :: bracketsStack

			while (!bracketsStack.isEmpty) {
				nextLexemeCheckEmpty()
				val value = lexeme.get.value
				if (openBrackets.contains(value)) {
					bracketsStack = value :: bracketsStack
				} else if (closeBrackets.contains(value)) {
					val stackTop = bracketsStack.head
					if (!isPairedBrackets(stackTop, value)) {
						sendError("Mismatched parentheses", lexeme.get.lineNumber)
					}
					bracketsStack = bracketsStack.tail
				}
			}
		}

		header()
		bracketsBalance()
		lexeme = lexemeTable.next()
	}
}