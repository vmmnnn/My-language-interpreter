package my

import org.scalatest.FunSuite


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
		if (lexeme1.lineNumber != lexeme2.lineNumber) return false
		if (lexeme1.lexemeType == LexemeType.DoubleNumber) {
			if (sameDoubleNumberStrings(lexeme1.value, lexeme2.value, eps) == false) {
				return false
			}
		} else {
			if (lexeme1.value != lexeme2.value) return false
		}
		true
	}

	def sameLexemeTables(table1: LexemeTable,
											 table2: LexemeTable,
											 eps: Double = 1e-10): Boolean = {
		if (table1.size != table2.size) return false
		(0 until table1.size).foreach(_ => {
			val lexeme1 = table1.next()
			val lexeme2 = table2.next()
			if (!sameLexemes(lexeme1.get, lexeme2.get, eps)) return false
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

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme("/", LexemeType.ArithmeticOp, 1))

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

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme(program, LexemeType.IntNumber, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("intNumber with comments to lexemes") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val numStr = "254"
		val program = "// comment 1\n" + numStr + "// comment"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme(numStr, LexemeType.IntNumber, 2))

		assert(sameLexemeTables(expected, lexemesTable, 1))
	}

	test("intNumber / intNumber: 254/2 to lexemes") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val num1 = "254"
		val num2 = "2"
		val program = num1 + "/" + num2

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme(num1, LexemeType.IntNumber, 1))
			.add(new Lexeme("/", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme(num2, LexemeType.IntNumber, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("doubleNumber 25.7 to lexeme") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "25.7"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme(program, LexemeType.DoubleNumber, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("doubleNumber 0.047 to lexeme") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "0.047"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme(program, LexemeType.DoubleNumber, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("2.09 *307") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val num1 = "2.09"
		val num2 = "307"
		val program = num1 + " *" + num2

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme(num1, LexemeType.DoubleNumber, 1))
			.add(new Lexeme("*", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme(num2, LexemeType.IntNumber, 1))

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

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme(num1, LexemeType.IntNumber, 1))
			.add(new Lexeme("+", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("(", LexemeType.Brackets, 1))
			.add(new Lexeme(num2, LexemeType.IntNumber, 1))
			.add(new Lexeme("/", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme(num3, LexemeType.IntNumber, 1))
			.add(new Lexeme("-", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme(num4, LexemeType.DoubleNumber, 1))
			.add(new Lexeme(")", LexemeType.Brackets, 1))
			.add(new Lexeme("%", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme(num5, LexemeType.IntNumber, 1))

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

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme(identifier1, LexemeType.Name, 1))
			.add(new Lexeme("+", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme(num, LexemeType.IntNumber, 1))
			.add(new Lexeme("*", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme(identifier2, LexemeType.Name, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("def main(): None") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "def main(): None"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme("def", LexemeType.KeyWord, 1))
			.add(new Lexeme("main", LexemeType.Name, 1))
			.add(new Lexeme("(", LexemeType.Brackets, 1))
			.add(new Lexeme(")", LexemeType.Brackets, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))
			.add(new Lexeme("None", LexemeType.Type, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("idx=0") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val identifier = "idx"
		val num = "0"
		val program = f"${identifier}=${num}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme(identifier, LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme(num, LexemeType.IntNumber, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("if (a != b) {idx=0}") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "if (a != b) {idx=0}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme("if", LexemeType.KeyWord, 1))
			.add(new Lexeme("(", LexemeType.Brackets, 1))
			.add(new Lexeme("a", LexemeType.Name, 1))
			.add(new Lexeme("!=", LexemeType.BoolOp, 1))
			.add(new Lexeme("b", LexemeType.Name, 1))
			.add(new Lexeme(")", LexemeType.Brackets, 1))
			.add(new Lexeme("{", LexemeType.Brackets, 1))
			.add(new Lexeme("idx", LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("0", LexemeType.IntNumber, 1))
			.add(new Lexeme("}", LexemeType.Brackets, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("if (a == b) {idx=0}") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "if (a == b) {idx=0}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme("if", LexemeType.KeyWord, 1))
			.add(new Lexeme("(", LexemeType.Brackets, 1))
			.add(new Lexeme("a", LexemeType.Name, 1))
			.add(new Lexeme("==", LexemeType.BoolOp, 1))
			.add(new Lexeme("b", LexemeType.Name, 1))
			.add(new Lexeme(")", LexemeType.Brackets, 1))
			.add(new Lexeme("{", LexemeType.Brackets, 1))
			.add(new Lexeme("idx", LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("0", LexemeType.IntNumber, 1))
			.add(new Lexeme("}", LexemeType.Brackets, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("if (a <= b) {c = a >b}") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "if (a <= b) {c = a >b}"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme("if", LexemeType.KeyWord, 1))
			.add(new Lexeme("(", LexemeType.Brackets, 1))
			.add(new Lexeme("a", LexemeType.Name, 1))
			.add(new Lexeme("<=", LexemeType.BoolOp, 1))
			.add(new Lexeme("b", LexemeType.Name, 1))
			.add(new Lexeme(")", LexemeType.Brackets, 1))
			.add(new Lexeme("{", LexemeType.Brackets, 1))
			.add(new Lexeme("c", LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("a", LexemeType.Name, 1))
			.add(new Lexeme(">", LexemeType.BoolOp, 1))
			.add(new Lexeme("b", LexemeType.Name, 1))
			.add(new Lexeme("}", LexemeType.Brackets, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("print('abc')") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "print(" + '"'.toString + "abc" + '"'.toString + ")"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme("print", LexemeType.LangFunction, 1))
			.add(new Lexeme("(", LexemeType.Brackets, 1))
			.add(new Lexeme("abc", LexemeType.String, 1))
			.add(new Lexeme(")", LexemeType.Brackets, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("print(22, '!=', 0.0)") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "print(22," + '"'.toString + "!=" + '"'.toString + " ,0.0)"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme("print", LexemeType.LangFunction, 1))
			.add(new Lexeme("(", LexemeType.Brackets, 1))
			.add(new Lexeme("22", LexemeType.IntNumber, 1))
			.add(new Lexeme(",", LexemeType.Comma, 1))
			.add(new Lexeme("!=", LexemeType.String, 1))
			.add(new Lexeme(",", LexemeType.Comma, 1))
			.add(new Lexeme("0.0", LexemeType.DoubleNumber, 1))
			.add(new Lexeme(")", LexemeType.Brackets, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

	test("arr: Array of Array of Double = [[2.0, 3.1], [9.2, 0.3, 4.1]]") {
		val lexicalAnalyzer = new LexicalAnalyzer
		val program = "arr: Array of Array of Double = [[2.0, 3.1], [9.2, 0.3, 4.1]]"

		lexicalAnalyzer.run(program)
		val lexemesTable = lexicalAnalyzer.lexemesTable

		val expected: LexemeTable = new LexemeTable()
			.add(new Lexeme("arr", LexemeType.Name, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))
			.add(new Lexeme("Array", LexemeType.Type, 1))
			.add(new Lexeme("of", LexemeType.KeyWord, 1))
			.add(new Lexeme("Array", LexemeType.Type, 1))
			.add(new Lexeme("of", LexemeType.KeyWord, 1))
			.add(new Lexeme("Double", LexemeType.Type, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("[", LexemeType.Brackets, 1))
			.add(new Lexeme("[", LexemeType.Brackets, 1))
			.add(new Lexeme("2.0", LexemeType.DoubleNumber, 1))
			.add(new Lexeme(",", LexemeType.Comma, 1))
			.add(new Lexeme("3.1", LexemeType.DoubleNumber, 1))
			.add(new Lexeme("]", LexemeType.Brackets, 1))
			.add(new Lexeme(",", LexemeType.Comma, 1))
			.add(new Lexeme("[", LexemeType.Brackets, 1))
			.add(new Lexeme("9.2", LexemeType.DoubleNumber, 1))
			.add(new Lexeme(",", LexemeType.Comma, 1))
			.add(new Lexeme("0.3", LexemeType.DoubleNumber, 1))
			.add(new Lexeme(",", LexemeType.Comma, 1))
			.add(new Lexeme("4.1", LexemeType.DoubleNumber, 1))
			.add(new Lexeme("]", LexemeType.Brackets, 1))
			.add(new Lexeme("]", LexemeType.Brackets, 1))

		assert(sameLexemeTables(expected, lexemesTable))
	}

}