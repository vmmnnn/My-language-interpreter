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

	def sameLexemes(lexeme1: Lexeme, lexeme2: Lexeme, eps: Double = 1e-10): Boolean = {
		if (lexeme1.lexemeType != lexeme2.lexemeType) return false
		if (lexeme1.lexemeType == LexemeType.DoubleNumber) {
			if (sameDoubleNumberStrings(lexeme1.value, lexeme2.value, eps) == false) {
				return false
			}
		} else {
			if (lexeme1.value != lexeme2.value) return false
		}
		true
	}

	def sameLexemeTables(table1: ArrayBuffer[Lexeme],
											 table2: ArrayBuffer[Lexeme],
											 eps: Double = 1e-10): Boolean = {
		if (table1.length != table2.length) return false
		table1.zip(table2).foreach(pair => if (!sameLexemes(pair._1, pair._2, eps)) return false)
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

		val expected: ArrayBuffer[Lexeme] =
			ArrayBuffer(new Lexeme("/", LexemeType.ArithmeticOp))

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

		val expected: ArrayBuffer[Lexeme] =
			ArrayBuffer(new Lexeme(program, LexemeType.IntNumber))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("intNumber with comments to lexemes") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val numStr = "254"
		val program = "// comment 1\n" + numStr + "// comment"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[Lexeme] =
			ArrayBuffer(new Lexeme(numStr, LexemeType.IntNumber))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("intNumber / intNumber: 254/2 to lexemes") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val num1 = "254"
		val num2 = "2"
		val program = num1 + "/" + num2

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[Lexeme] = ArrayBuffer(
				new Lexeme(num1, LexemeType.IntNumber),
				new Lexeme("/", LexemeType.ArithmeticOp),
				new Lexeme(num2, LexemeType.IntNumber)
			)

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("doubleNumber 25.7 to lexeme") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "25.7"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[Lexeme] =
			ArrayBuffer(new Lexeme(program, LexemeType.DoubleNumber))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("doubleNumber 0.047 to lexeme") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "0.047"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[Lexeme] =
			ArrayBuffer(new Lexeme(program, LexemeType.DoubleNumber))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("2.09 *307") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val num1 = "2.09"
		val num2 = "307"
		val program = num1 + " *" + num2

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[Lexeme] = ArrayBuffer(
			new Lexeme(num1, LexemeType.DoubleNumber),
			new Lexeme("*", LexemeType.ArithmeticOp),
			new Lexeme(num2, LexemeType.IntNumber))

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

		val expected: ArrayBuffer[Lexeme] = ArrayBuffer(
			new Lexeme(num1, LexemeType.IntNumber),
			new Lexeme("+", LexemeType.ArithmeticOp),
			new Lexeme("(", LexemeType.Brackets),
			new Lexeme(num2, LexemeType.IntNumber),
			new Lexeme("/", LexemeType.ArithmeticOp),
			new Lexeme(num3, LexemeType.IntNumber),
			new Lexeme("-", LexemeType.ArithmeticOp),
			new Lexeme(num4, LexemeType.DoubleNumber),
			new Lexeme(")", LexemeType.Brackets),
			new Lexeme("%", LexemeType.ArithmeticOp),
			new Lexeme(num5, LexemeType.IntNumber))

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

		val expected: ArrayBuffer[Lexeme] = ArrayBuffer(
			new Lexeme(identifier1, LexemeType.Name),
			new Lexeme("+", LexemeType.ArithmeticOp),
			new Lexeme(num, LexemeType.IntNumber),
			new Lexeme("*", LexemeType.ArithmeticOp),
			new Lexeme(identifier2, LexemeType.Name))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("def main(): None") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "def main(): None"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[Lexeme] = ArrayBuffer(
			new Lexeme("def", LexemeType.KeyWord),
			new Lexeme("main", LexemeType.Name),
			new Lexeme("(", LexemeType.Brackets),
			new Lexeme(")", LexemeType.Brackets),
			new Lexeme(":", LexemeType.Colon),
			new Lexeme("None", LexemeType.Type))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("idx=0") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val identifier = "idx"
		val num = "0"
		val program = f"${identifier}=${num}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[Lexeme] = ArrayBuffer(
			new Lexeme(identifier, LexemeType.Name),
			new Lexeme("=", LexemeType.DefineOp),
			new Lexeme(num, LexemeType.IntNumber))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("if (a != b) {idx=0}") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "if (a != b) {idx=0}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[Lexeme] = ArrayBuffer(
			new Lexeme("if", LexemeType.KeyWord),
			new Lexeme("(", LexemeType.Brackets),
			new Lexeme("a", LexemeType.Name),
			new Lexeme("!=", LexemeType.BoolOp),
			new Lexeme("b", LexemeType.Name),
			new Lexeme(")", LexemeType.Brackets),
			new Lexeme("{", LexemeType.Brackets),
			new Lexeme("idx", LexemeType.Name),
			new Lexeme("=", LexemeType.DefineOp),
			new Lexeme("0", LexemeType.IntNumber),
			new Lexeme("}", LexemeType.Brackets))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("if (a == b) {idx=0}") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "if (a == b) {idx=0}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[Lexeme] = ArrayBuffer(
			new Lexeme("if", LexemeType.KeyWord),
			new Lexeme("(", LexemeType.Brackets),
			new Lexeme("a", LexemeType.Name),
			new Lexeme("==", LexemeType.BoolOp),
			new Lexeme("b", LexemeType.Name),
			new Lexeme(")", LexemeType.Brackets),
			new Lexeme("{", LexemeType.Brackets),
			new Lexeme("idx", LexemeType.Name),
			new Lexeme("=", LexemeType.DefineOp),
			new Lexeme("0", LexemeType.IntNumber),
			new Lexeme("}", LexemeType.Brackets))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("if (a <= b) {c = a >b}") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "if (a <= b) {c = a >b}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[Lexeme] = ArrayBuffer(
			new Lexeme("if", LexemeType.KeyWord),
			new Lexeme("(", LexemeType.Brackets),
			new Lexeme("a", LexemeType.Name),
			new Lexeme("<=", LexemeType.BoolOp),
			new Lexeme("b", LexemeType.Name),
			new Lexeme(")", LexemeType.Brackets),
			new Lexeme("{", LexemeType.Brackets),
			new Lexeme("c", LexemeType.Name),
			new Lexeme("=", LexemeType.DefineOp),
			new Lexeme("a", LexemeType.Name),
			new Lexeme(">", LexemeType.BoolOp),
			new Lexeme("b", LexemeType.Name),
			new Lexeme("}", LexemeType.Brackets))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("print('abc')") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "print(" + '"'.toString + "abc" + '"'.toString + ")"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[Lexeme] = ArrayBuffer(
			new Lexeme("print", LexemeType.LangFunction),
			new Lexeme("(", LexemeType.Brackets),
			new Lexeme("abc", LexemeType.String),
			new Lexeme(")", LexemeType.Brackets))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("print(22, '!=', 0.0)") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "print(22," + '"'.toString + "!=" + '"'.toString + " ,0.0)"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: ArrayBuffer[Lexeme] = ArrayBuffer(
			new Lexeme("print", LexemeType.LangFunction),
			new Lexeme("(", LexemeType.Brackets),
			new Lexeme("22", LexemeType.IntNumber),
			new Lexeme(",", LexemeType.Comma),
			new Lexeme("!=", LexemeType.String),
			new Lexeme(",", LexemeType.Comma),
			new Lexeme("0.0", LexemeType.DoubleNumber),
			new Lexeme(")", LexemeType.Brackets))

		assert(sameLexemeTables(expected, lexemesTable))
	}

}