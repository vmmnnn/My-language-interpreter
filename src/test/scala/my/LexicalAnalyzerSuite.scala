package my

import org.scalatest.FunSuite

import scala.collection.mutable.ArrayBuffer


class LexicalAnalyzerSuite extends FunSuite {
	def sameLexemTables(table1: ArrayBuffer[(String, lexemeType.Value)],
														table2: ArrayBuffer[(String, lexemeType.Value)]): Boolean = {
		table1.zip(table2).foreach(pair => {
			if (pair._1._1 != pair._2._1) return false
			if (pair._1._2 != pair._2._2) return false
		})
		true
	}

	test("comment: // comment") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "// comment"
		lexicalAnalyzer.run(program)
		assert(lexicalAnalyzer.getState() == States.CommentLine)
	}

	test("comment: /// comment") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "/// comment"
		lexicalAnalyzer.run(program)
		assert(lexicalAnalyzer.getState() == States.CommentLine)
	}

	test("comment: / //comment") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "/ //comment"
		lexicalAnalyzer.run(program)
		assert(lexicalAnalyzer.getState() == States.SkipLine)
	}

	test("comment: // /comment") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "// /comment"
		lexicalAnalyzer.run(program)
		assert(lexicalAnalyzer.getState() == States.CommentLine)
	}

	test("lineNumber calculation check") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "def main(): None {\n" +
			"// comment\n" +
			"println(100 + 400)\n" +
			"}"
		lexicalAnalyzer.run(program)
		assert(lexicalAnalyzer.getLineNumber() == 4)
	}

	test("symbolNumber calculation check") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val programLine1 = "def main(): None {\n"
		val programLine2 = "// comment\n"
		val programLine3 = "println(100"
		val program = programLine1 + programLine2 + programLine3
		lexicalAnalyzer.run(program)
		assert(lexicalAnalyzer.getSymbolNumber() == programLine3.length)
	}

	test("intNumber to lexeme") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "254"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] =
			ArrayBuffer((program, lexemeType.IntNumber))

		assert(sameLexemTables(expected, lexemesTable))
	}

	test("intNumber with comments to lexemes") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val numStr = "254"
		val program = "// comment 1\n" + numStr + "    // comment"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] =
			ArrayBuffer((numStr, lexemeType.IntNumber))

		assert(sameLexemTables(expected, lexemesTable))
	}

}