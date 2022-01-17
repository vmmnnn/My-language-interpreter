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

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("0+(307/ 2-0.1 )%7") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val num1 = "0"
		val num2 = "307"
		val num3 = "2"
		val num4 = "0.1"
		val num5 = "7"

		val program = "0+(307/ 2-0.1 )%7"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] = ArrayBuffer(
			(num1, lexemeType.IntNumber),
			("+", lexemeType.ArithmeticOp),
			("(", lexemeType.Brackets),
			(num2, lexemeType.IntNumber),
			("/", lexemeType.ArithmeticOp),
			(num3, lexemeType.IntNumber),
			("-", lexemeType.ArithmeticOp),
			(num4, lexemeType.DoubleNumber),
			(")", lexemeType.Brackets),
			("%", lexemeType.ArithmeticOp),
			(num5, lexemeType.IntNumber))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("idx1+3*idx2") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val identifier1 = "idx1"
		val identifier2 = "idx2"
		val num = "3"
		val program = f"${identifier1}+${num}*${identifier2}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] = ArrayBuffer(
			(identifier1, lexemeType.Name),
			("+", lexemeType.ArithmeticOp),
			(num, lexemeType.IntNumber),
			("*", lexemeType.ArithmeticOp),
			(identifier2, lexemeType.Name))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("def main(): None") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "def main(): None"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] = ArrayBuffer(
			("def", lexemeType.KeyWord),
			("main", lexemeType.Name),
			("(", lexemeType.Brackets),
			(")", lexemeType.Brackets),
			(":", lexemeType.Colon),
			("None", lexemeType.Type))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("idx=0") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val identifier = "idx"
		val num = "0"
		val program = f"${identifier}=${num}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] = ArrayBuffer(
			(identifier, lexemeType.Name),
			("=", lexemeType.DefineOp),
			(num, lexemeType.IntNumber))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("if (a != b) {idx=0}") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "if (a != b) {idx=0}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] = ArrayBuffer(
			("if", lexemeType.KeyWord),
			("(", lexemeType.Brackets),
			("a", lexemeType.Name),
			("!=", lexemeType.BoolOp),
			("b", lexemeType.Name),
			(")", lexemeType.Brackets),
			("{", lexemeType.Brackets),
			("idx", lexemeType.Name),
			("=", lexemeType.DefineOp),
			("0", lexemeType.IntNumber),
			("}", lexemeType.Brackets))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("if (a == b) {idx=0}") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "if (a == b) {idx=0}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] = ArrayBuffer(
			("if", lexemeType.KeyWord),
			("(", lexemeType.Brackets),
			("a", lexemeType.Name),
			("==", lexemeType.BoolOp),
			("b", lexemeType.Name),
			(")", lexemeType.Brackets),
			("{", lexemeType.Brackets),
			("idx", lexemeType.Name),
			("=", lexemeType.DefineOp),
			("0", lexemeType.IntNumber),
			("}", lexemeType.Brackets))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("if (a <= b) {c = a >b}") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "if (a <= b) {c = a >b}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] = ArrayBuffer(
			("if", lexemeType.KeyWord),
			("(", lexemeType.Brackets),
			("a", lexemeType.Name),
			("<=", lexemeType.BoolOp),
			("b", lexemeType.Name),
			(")", lexemeType.Brackets),
			("{", lexemeType.Brackets),
			("c", lexemeType.Name),
			("=", lexemeType.DefineOp),
			("a", lexemeType.Name),
			(">", lexemeType.BoolOp),
			("b", lexemeType.Name),
			("}", lexemeType.Brackets))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("print('abc')") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "print(" + '"'.toString + "abc" + '"'.toString + ")"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] = ArrayBuffer(
			("print", lexemeType.LangFunction),
			("(", lexemeType.Brackets),
			("abc", lexemeType.String),
			(")", lexemeType.Brackets))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("print(22, '!=', 0.0)") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "print(22," + '"'.toString + "!=" + '"'.toString + " ,0.0)"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[(String, lexemeType.Value)] = ArrayBuffer(
			("print", lexemeType.LangFunction),
			("(", lexemeType.Brackets),
			("22", lexemeType.IntNumber),
			(",", lexemeType.Comma),
			("!=", lexemeType.String),
			(",", lexemeType.Comma),
			("0.0", lexemeType.DoubleNumber),
			(")", lexemeType.Brackets))

		assert(sameLexemeTables(expected, lexemesTable))
	}

}