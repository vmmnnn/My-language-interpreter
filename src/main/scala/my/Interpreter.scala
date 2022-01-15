package my

import my.lexemeType.{ArithmeticOp, BoolOp, BoolVal, DoubleNumber, Identifier, IntNumber, KeyWord, LangFunction, Type}

import scala.io.Source.fromFile


object Interpreter extends Serializable {
	def main(args: Array[String]): Unit = {
		var fileName = "examples/program.txt"
		args.sliding(2, 2).collect {
			case Array("--fileName", value) => fileName = value
		}
		val program = fromFile(fileName).mkString
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesTable = lexicalAnalyzer.lexemesTable
		lexemesTable.foreach(pair => {
			print(pair._1 + " - ")
			pair._2 match {
				case ArithmeticOp => println("ArithmeticOp")
				case BoolOp => println("BoolOp")
				case BoolVal => println("BoolVal")
				case DoubleNumber => println("DoubleNumber")
				case LangFunction => println("LangFunction")
				case IntNumber => println("IntNumber")
				case Type => println("Type")
				case KeyWord => println("KeyWord")
				case Identifier => println("Identifier")
			}
		})
	}
}