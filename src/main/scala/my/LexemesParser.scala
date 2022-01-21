package my

import my.LexemeType.{BoolVal, DoubleNumber, IntNumber}

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
/*
class Variable(val name: String, val varType: VarType.Value, var value: Option[String]) {
	def isSame(otherVariable: Variable): Boolean = {
		if (name != otherVariable.name) return false
		if (value != otherVariable.value) return false
		if (varType != otherVariable.varType) return false
		true
	}

	override def toString: String = {
		if (value.isEmpty) f"$name: ${varType}"
		else f"$name: ${varType} = ${value.get}"
	}
}
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

class LexemesParser(lexemeTable: LexemeTable) {
	private var lexeme = lexemeTable.next()
	private val globalVars = new VarTable

	def getGlobalVars: VarTable = globalVars

	def run(): Unit = {
		// program starts either from global variables or right from functions
		while (lexeme.isDefined) {
			if (lexeme.get.lexemeType == LexemeType.Name) {  // global variables
				val (name, value) = getVariable()
				globalVars.setVal(name, value)
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

	private def isValue(lexeme: Lexeme): Boolean = {
		lexeme.lexemeType match {
			case BoolVal => true
			case DoubleNumber => true
			case IntNumber => true
			case LexemeType.String => true
			case _ => false
		}
	}

	private def getVariable(): (String, Value) = {
		val varName = lexeme.get.value
		var varType: Option[VarType.Value] = None
		var varValue: Option[String] = None

		def getValueCheckType(): Unit = {
			varValue = Option(lexeme.get.value)
			val lineNumber = lexeme.get.lineNumber
			lexeme.get.lexemeType match {
				case BoolVal if varType.get == VarType.Bool =>
				case DoubleNumber if varType.get == VarType.Double =>
				case IntNumber if varType.get == VarType.Int =>
				case IntNumber if varType.get == VarType.Double =>
				case LexemeType.String if varType.get == VarType.String =>
				case _ =>
					sendError(f"Types do not match: ${varType.get} expected, but ${lexeme.get.lexemeType} found", lineNumber)
			}
		}

		def getValueSetType(): Unit = {
			if (!isValue(lexeme.get)) {
				sendError(f"Unexpected symbol ${lexeme.get.value}", lexeme.get.lineNumber)
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
				sendError(f"Unknown token '${varName}'", lineNumber)
			}
			return (varName, new Value(varType.get, None))
		}

		// Neither colon for type nor '=' for value were found
		if (varType.isEmpty & (lexeme.get.lexemeType != LexemeType.DefineOp)) {
			sendError(f"Unknown token '${varName}'", lineNumber)
		}

		// '=' => look for a value
		if (lexeme.get.lexemeType == LexemeType.DefineOp) {
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
}