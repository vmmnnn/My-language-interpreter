package my


class Interpreter(val program: String) {
	var lexemesParser: Option[LexemesParser] = None
	def parse(): Interpreter = {
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		lexemesParser = Option(new LexemesParser(lexicalAnalyzer.lexemesTable))
		lexemesParser.get.parse()

		this
	}

	def run(mainFunction: String = "main"): Unit = {
		if (lexemesParser.isEmpty) {
			System.err.println(f"First, parse function should be called")
		} else {
			lexemesParser.get.run(mainFunction)
		}
	}
}