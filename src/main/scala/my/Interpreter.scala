package my

import scala.io.Source.fromFile


object Main extends Serializable {
	def main(args: Array[String]): Unit = {
		var fileName = "examples/program.txt"
		args.sliding(2, 2).collect {
			case Array("--fileName", value) => fileName = value
		}
		val program = fromFile(fileName).mkString

		val interpreter = new Interpreter(program)
		interpreter.run()
	}
}

class Interpreter(val program: String) {
	def run(): Unit = {
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.run()
	}
}