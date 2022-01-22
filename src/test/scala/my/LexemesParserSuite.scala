package my

import org.scalatest.FunSuite

import scala.collection.mutable.Map


class LexemesParserSuite extends FunSuite {
	def sameVarTables(table1: VarTable, table2: VarTable): Boolean = {
		val keys1 = table1.getTable.keys.toArray
		val keys2 = table2.getTable.keys.toArray
		if (!keys1.sameElements(keys2)) return false
		keys1.foreach{ case name =>
			val variable1 = table1.getVal(name).get
			val variable2 = table2.getVal(name).get
			if (!variable1.isSame(variable2)) return false
		}
		true
	}

	def sameFunctionTables(table1: Map[String, Int], table2: Map[String, Int]): Boolean = {
		val keys1 = table1.keys.toArray
		val keys2 = table2.keys.toArray
		if (!keys1.sameElements(keys2)) return false
		keys1.foreach{ case name =>
			val val1 = table1(name)
			val val2 = table2(name)
			if (val1 != val2) return false
		}
		true
	}

	test("idx = 3") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("idx", LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("3", LexemeType.IntNumber, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		lexemesParser.parse()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Int, Option("3"))
		expected.setVal("idx", expectedVar)

		assert(sameVarTables(expected, globalVars))
	}

	test("x: Double = 3") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("x", LexemeType.Name, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))
			.add(new Lexeme("Double", LexemeType.Type, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("3", LexemeType.IntNumber, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		lexemesParser.parse()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Double, Option("3"))
		expected.setVal("x", expectedVar)

		assert(sameVarTables(expected, globalVars))
	}

	test("idx: Bool = 3") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("idx", LexemeType.Name, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))
			.add(new Lexeme("Bool", LexemeType.Type, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("3", LexemeType.IntNumber, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("s: String") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("s", LexemeType.Name, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))
			.add(new Lexeme("String", LexemeType.Type, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		lexemesParser.parse()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.String, None)
		expected.setVal("s", expectedVar)

		assert(sameVarTables(expected, globalVars))
	}

	test("x = >") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("x", LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme(">", LexemeType.BoolOp, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("x = ") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("x", LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("x : ") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("x", LexemeType.Name, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("x") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("x", LexemeType.Name, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("x = 25; fl: Bool = True") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("x", LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("25", LexemeType.IntNumber, 1))
			.add(new Lexeme("fl", LexemeType.Name, 2))
			.add(new Lexeme(":", LexemeType.Colon, 2))
			.add(new Lexeme("Bool", LexemeType.Type, 2))
			.add(new Lexeme("=", LexemeType.DefineOp, 2))
			.add(new Lexeme("True", LexemeType.BoolVal, 2))

		val lexemesParser = new LexemesParser(lexemeTable)
		lexemesParser.parse()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar1: Value = new Value(VarType.Int, Option("25"))
		val expectedVar2: Value = new Value(VarType.Bool, Option("True"))
		expected.setVal("x", expectedVar1)
		expected.setVal("fl", expectedVar2)

		globalVars.print()

		assert(sameVarTables(expected, globalVars))
	}

	test("def func1(): None {}") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("def", LexemeType.KeyWord, 1))
			.add(new Lexeme("func1", LexemeType.Name, 1))
			.add(new Lexeme("(", LexemeType.Brackets, 1))
			.add(new Lexeme(")", LexemeType.Brackets, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))
			.add(new Lexeme("None", LexemeType.Type, 1))
			.add(new Lexeme("{", LexemeType.Brackets, 1))
			.add(new Lexeme("}", LexemeType.Brackets, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		lexemesParser.parse()

		val functionTable = lexemesParser.getFunctionTable
		val expectedFunctionTable: Map[String, Int] = Map("func1" -> 2)
		assert(sameFunctionTables(expectedFunctionTable, functionTable))
	}

	test("2 functions with parameters") {
		val program = "def func1(p1: Int, p2: Double): Bool {\n" +
			"x = (2+2)/2\n" +
			"if (func2(p1) == 1) {return False}\n" +
			"return True\n" +
			"}\n" +
			"p = 0\n" +
			"def func2(p3: Int): Double {return (p+p3)/p3}\n"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse()

		val globalVars = lexemesParser.getGlobalVars
		val functionTable = lexemesParser.getFunctionTable

		val expectedGlobalVars: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Int, Option("0"))
		expectedGlobalVars.setVal("p", expectedVar)

		val expectedFunctionTable: Map[String, Int] = Map("func1" -> 2, "func2" -> 44)

		assert(sameVarTables(expectedGlobalVars, globalVars))
		assert(sameFunctionTables(expectedFunctionTable, functionTable))
	}

	test("2 functions with parameters and main function") {
		val program = "def func1(p1: Int, p2: Double): Bool {\n" +
			"x = (2+2)/2\n" +
			"if (func2(p1) == 1) {return False}\n" +
			"return True\n" +
			"}\n" +
			"def main(): None {print(func1(2, 3))}" +
			"p = 0\n" +
			"def func2(p3: Int): Double {return (p+p3)/p3}\n"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse()

		val globalVars = lexemesParser.getGlobalVars
		val functionTable = lexemesParser.getFunctionTable

		val expectedGlobalVars: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Int, Option("0"))
		expectedGlobalVars.setVal("p", expectedVar)

		val expectedFunctionTable: Map[String, Int] = Map("func1" -> 2, "func2" -> 61, "main" -> 41)

		assert(sameVarTables(expectedGlobalVars, globalVars))
		assert(sameFunctionTables(expectedFunctionTable, functionTable))
	}

	test("Function type missed") {
		val program = "def f(): {}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("Colon missed") {
		val program = "def f() Int {}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("'def' missed") {
		val program = "f(): Int {}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("Function name missed") {
		val program = "def (): Int {}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("Parameter open bracket missed") {
		val program = "def f): Int {}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("Parameter close bracket missed") {
		val program = "def f(: Int {}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("Parameter brackets missed") {
		val program = "def f: Int {}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("Parameter code open brackets missed") {
		val program = "def f(): Int x=3}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("Parameter code close brackets missed") {
		val program = "def f(): Int {x=3"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("Brackets mismatched: no ')'") {
		val program = "def f(): Int {x=(3}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("Brackets mismatched: no '('") {
		val program = "def f(): Int {x=3)}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("Brackets mismatched: no '}'") {
		val program = "def f(): Int {x={3}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("Brackets mismatched: no '{'") {
		val program = "def f(): Int {x=3}}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		intercept[Exception] { lexemesParser.parse() }
	}

	test("Quotes not closed") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("def", LexemeType.KeyWord, 1))
			.add(new Lexeme("f", LexemeType.Name, 1))
			.add(new Lexeme("(", LexemeType.Brackets, 1))
			.add(new Lexeme(")", LexemeType.Brackets, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))
			.add(new Lexeme("None", LexemeType.Type, 1))
			.add(new Lexeme("{", LexemeType.Brackets, 1))
			.add(new Lexeme("s", LexemeType.Name, 2))
			.add(new Lexeme("=", LexemeType.DefineOp, 2))
			.add(new Lexeme("abc}}", LexemeType.String, 2))

		val lexemesParser = new LexemesParser(lexemeTable)
		intercept[Exception] { lexemesParser.parse() }
	}

}