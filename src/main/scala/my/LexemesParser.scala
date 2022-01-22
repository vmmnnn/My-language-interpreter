package my

import com.sun.org.apache.bcel.internal.classfile.LineNumber
import my.LexemeType.{BoolVal, Brackets, Colon, Comma, DoubleNumber, IntNumber, Name, Type}

import scala.collection.mutable.Map


object VarType extends Enumeration {
	type VarType = Value
	val Array, Bool, Double, Int, String = Value
	def getType(str: String): VarType.Value = {
		str match {
			case "Array" => Array
			case "Bool" => Bool
			case "Double" => Double
			case "Int" => Int
			case "String" => String
		}
	}
}

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
 * Parses lexemes given and runs the program that starts from main function, usually 'main'<br>
 * Main function should have no parameters and return None
 * @param lexemeTable result of lexical analysis
 * @param mainFunction function to be run, usually 'main'
 */
class LexemesParser(lexemeTable: LexemeTable, mainFunction: String = "main") {
	// current lexeme, updates during parsing
	private var lexeme = lexemeTable.next()

	// table with global variables
	private val globalVars = new VarTable

	// table of functions: name, 'def'-lexeme position in LexemeTable
	private val functionTable: Map[String, Int] = Map.empty

	def getGlobalVars: VarTable = globalVars
	def getFunctionTable: Map[String, Int] = functionTable

	def run(): Unit = {
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
	}

	/**
	 * Sends a message to stderr and throws an Exception
	 * @param message message to be printed
	 */
	private def sendError(message: String, lineNumber: Int): Unit = {
		System.err.println(f"line ${lineNumber}: $message")
		throw new Exception
	}

	/**
	 * Sends "Unexpected token '...'" message with current lexeme
	 */
	private def sendUnexpectedTokenError(): Unit = {
		sendError(f"Unexpected token ${lexeme.get.value}", lexeme.get.lineNumber)
	}

	/**
	 * Sends "Unexpected token '...'" message with token and position given
	 * @param token wrong lexeme found
	 * @param lineNumber position
	 */
	private def sendUnexpectedTokenError(token: String, lineNumber: Int): Unit = {
		sendError(f"Unexpected token $token", lineNumber)
	}

	/**
	 * Sends "'...' expected, but '...' found" message
	 * @param expected what was expected
	 */
	private def sendExpectedFoundError(expected: String): Unit = {
		sendError(f"$expected expected, but ${lexeme.get.value} found", lexeme.get.lineNumber)
	}

	/**
	 * Takes next lexeme from lexeme table when it has to exist
	 */
	private def nextLexemeCheckEmpty(): Unit = {
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
	private def getVarType(): VarType.Value = {
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
	private def isValue(lexeme: Lexeme): Boolean = {
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
	private def getVariable(): (String, Value) = {
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
	 * If it is 'main' function - run it<br>
	 * Otherwise save it in function table to call it if needed
	 */
	private def parseFunction(): Unit = {
		checkNextLexemeType(Name, "Function name")
		val name = lexeme.get.value
		if (name == mainFunction) {
			runMain()
		} else {
			val checkIfExists = functionTable.get(name)
			if (checkIfExists.isDefined) {  // the function has been already defined
				val errorMessage = f"Duplicated function name $name."
				val previousDefinitionMessage = f"Previous declaration in line $checkIfExists"
				sendError(f"$errorMessage\n$previousDefinitionMessage", lexeme.get.lineNumber)
			}
			functionTable(name) = lexeme.get.lineNumber
			skipFunction()
		}
	}

	/**
	 * Main function should have no parameters and return None<br>
	 * Should be called when current lexeme is function name
	 * @return true/false - is the header right
	 */
	private def checkMainHeader(): Boolean = {
		checkNextLexemeValue("(")
		checkNextLexemeValue(")")
		true  // TODO: finish function
	}

	/**
	 * Runs main function<br>
	 * Called when current lexeme is a function name
	 */
	private def runMain(): Unit = {

	}

	/**
	 * Gets next lexeme and check its type
	 * @param expected expected type
	 * @param expectedErrorMessage what was expected (to be printed in expected-found error)
	 */
	private def checkNextLexemeType(expected: LexemeType.Value, expectedErrorMessage: String): Unit = {
		nextLexemeCheckEmpty()
		if (lexeme.get.lexemeType != expected) {
			sendExpectedFoundError(expectedErrorMessage)
		}
	}

	/**
	 * Gets next lexeme and check if we got token that was expected
	 * @param expected expected token
	 */
	private def checkNextLexemeValue(expected: String): Unit = {
		nextLexemeCheckEmpty()
		if (lexeme.get.value != expected) {
			sendExpectedFoundError(f"'$expected'")
		}
	}

	/**
	 * Skips lexemes until function ends (def name(...): Type {...})<br>
	 * Should be called when lexeme is a function name
 	 */
	private def skipFunction(): Unit = {
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