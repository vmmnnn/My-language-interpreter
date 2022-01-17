package my

import org.scalatest.FunSuite

import scala.collection.mutable.ArrayBuffer


class LexicalAnalyzerSuite extends FunSuite {
	def isDoubleNumberString(str: String): Boolean = {
		if (str.count(s => s == '.') != 1) return false
		if (str.count(s => s.isDigit) != str.length -1) return false
		true
	}

	def sameDoubleNumberStrings(str1: String, str2: String, eps: Double = 1e-10): Boolean = {
		if (!isDoubleNumberString(str1)) return false
		if (!isDoubleNumberString(str2)) return false
		if (str1.toDouble - str2.toDouble > eps) return false
		true
	}

	def sameLexemeTables(table1: ArrayBuffer[(String, lexemeType.Value)],
											 table2: ArrayBuffer[(String, lexemeType.Value)],
											 eps: Double = 1e-10): Boolean = {
		if (table1.length != table2.length) return false
		table1.zip(table2).foreach(pair => {
			if (pair._1._2 != pair._2._2) return false
			if (pair._1._2 == lexemeType.DoubleNumber) {
				if (sameDoubleNumberStrings(pair._1._1, pair._2._1, eps) == false) {
					return false
				}
			} else {
				if (pair._1._1 != pair._2._1) return false
			}
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

		val expected: ArrayBuffer[(String, lexemeType.Value)] =
			ArrayBuffer(("/", lexemeType.ArithmeticOp))

		assert(lexicalAnalyzer.getState() == States.CommentLine)
		assert(sameLexemeTables(expected, lexicalAnalyzer.lexemesTable))
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
		val expected = 4
		lexicalAnalyzer.run(program)
		assert(lexicalAnalyzer.getLineNumber() == expected)
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

	test("intNumber 254 to lexeme") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "254"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] =
			ArrayBuffer((program, lexemeType.IntNumber))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("intNumber with comments to lexemes") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val numStr = "254"
		val program = "// comment 1\n" + numStr + "// comment"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] =
			ArrayBuffer((numStr, lexemeType.IntNumber))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("intNumber / intNumber: 254/2 to lexemes") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val num1 = "254"
		val num2 = "2"
		val program = num1 + "/" + num2

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] = ArrayBuffer(
				(num1, lexemeType.IntNumber),
				("/", lexemeType.ArithmeticOp),
				(num2, lexemeType.IntNumber)
			)

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("doubleNumber 25.7 to lexeme") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "25.7"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] =
			ArrayBuffer((program, lexemeType.DoubleNumber))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("doubleNumber 0.047 to lexeme") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "0.047"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] =
			ArrayBuffer((program, lexemeType.DoubleNumber))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("2.09 *307") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val num1 = "2.09"
		val num2 = "307"
		val program = num1 + " *" + num2

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] =
			ArrayBuffer((num1, lexemeType.DoubleNumber),
									("*", lexemeType.ArithmeticOp),
									(num2, lexemeType.IntNumber))
		lexicalAnalyzer.printLexemeTable()
		assert(sameLexemeTables(expected, lexemesTable))
	}
}