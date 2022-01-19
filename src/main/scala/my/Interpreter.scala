package my

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

		lexicalAnalyzer.lexemesTable.print()
	}
}