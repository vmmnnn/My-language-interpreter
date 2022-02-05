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

	test("idx = 3;") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("idx", LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("3", LexemeType.IntNumber, 1))
			.add(new Lexeme(";", LexemeType.Semicolon, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		lexemesParser.parse()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Int, Option("3"))
		expected.setVal("idx", expectedVar)

		assert(sameVarTables(expected, globalVars))
	}

	test("idx = 7*3+1;") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("idx", LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("7", LexemeType.IntNumber, 1))
			.add(new Lexeme("*", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("3", LexemeType.IntNumber, 1))
			.add(new Lexeme("+", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("1", LexemeType.IntNumber, 1))
			.add(new Lexeme(";", LexemeType.Semicolon, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		lexemesParser.parse()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Int, Option("22"))
		expected.setVal("idx", expectedVar)

		assert(sameVarTables(expected, globalVars))
	}

	test("x: Double = 3/2;") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("x", LexemeType.Name, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))
			.add(new Lexeme("Double", LexemeType.Type, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("3", LexemeType.IntNumber, 1))
			.add(new Lexeme("/", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("2", LexemeType.IntNumber, 1))
			.add(new Lexeme(";", LexemeType.Semicolon, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		lexemesParser.parse()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Double, Option("1.5"))
		expected.setVal("x", expectedVar)

		assert(sameVarTables(expected, globalVars))
	}

	test("idx: Bool = 3;") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("idx", LexemeType.Name, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))
			.add(new Lexeme("Bool", LexemeType.Type, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("3", LexemeType.IntNumber, 1))
			.add(new Lexeme(";", LexemeType.Semicolon, 1))

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

	test("x = 25; fl: Bool = True;") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("x", LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("25", LexemeType.IntNumber, 1))
			.add(new Lexeme(";", LexemeType.Semicolon, 1))
			.add(new Lexeme("fl", LexemeType.Name, 2))
			.add(new Lexeme(":", LexemeType.Colon, 2))
			.add(new Lexeme("Bool", LexemeType.Type, 2))
			.add(new Lexeme("=", LexemeType.DefineOp, 2))
			.add(new Lexeme("True", LexemeType.BoolVal, 2))
			.add(new Lexeme(";", LexemeType.Semicolon, 2))

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
			"x = (2+2)/2;\n" +
			"if (func2(p1) == 1;) {return False;}\n" +
			"return True;\n" +
			"}\n" +
			"p = 0;\n" +
			"def func2(p3: Int): Double {return (p+p3)/p3;}\n"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse()

		val globalVars = lexemesParser.getGlobalVars
		val functionTable = lexemesParser.getFunctionTable

		val expectedGlobalVars: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Int, Option("0"))
		expectedGlobalVars.setVal("p", expectedVar)

		val expectedFunctionTable: Map[String, Int] = Map("func1" -> 2, "func2" -> 49)

		assert(sameVarTables(expectedGlobalVars, globalVars))
		assert(sameFunctionTables(expectedFunctionTable, functionTable))
	}

	test("2 functions with parameters and main function") {
		val program = "def func1(p1: Int, p2: Double): Bool {\n" +
			"x = (2+2)/2;\n" +
			"if (func2(p1) == 1;) {return False;}\n" +
			"return True;\n" +
			"}\n" +
			"def main(): None {print(func1(2, 3);)}" +
			"p = 0;\n" +
			"def func2(p3: Int): Double {return (p+p3)/p3;}\n"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse()

		val globalVars = lexemesParser.getGlobalVars
		val functionTable = lexemesParser.getFunctionTable

		val expectedGlobalVars: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Int, Option("0"))
		expectedGlobalVars.setVal("p", expectedVar)

		val expectedFunctionTable: Map[String, Int] = Map("func1" -> 2, "func2" -> 67, "main" -> 45)

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

	test("def main(): Int {}") {
		val program = "def main(): Int {}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse()
		intercept[Exception] { lexemesParser.run() }
	}

	test("def main(p: Double): None {}") {
		val program = "def main(p: Double): None {}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse()
		intercept[Exception] { lexemesParser.run() }
	}

	test("def f(p: Double): None {}") {
		val program = "def f(p: Double): None {}"
		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse()
		intercept[Exception] { lexemesParser.run("f") }
	}

	test("compute: 3-2+5;") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("3", LexemeType.IntNumber, 1))
			.add(new Lexeme("-", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("2", LexemeType.IntNumber, 1))
			.add(new Lexeme("+", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("5", LexemeType.IntNumber, 1))
			.add(new Lexeme(";", LexemeType.Semicolon, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		val res = lexemesParser.compute(new VarTable)

		val expected = new Value(VarType.Int, Option("6"))

		assert(res.isSame(expected))
	}

	test("compute: 1 > 2;") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("1", LexemeType.IntNumber, 1))
			.add(new Lexeme(">", LexemeType.BoolOp, 1))
			.add(new Lexeme("2", LexemeType.IntNumber, 1))
			.add(new Lexeme(";", LexemeType.Semicolon, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		val res = lexemesParser.compute(new VarTable)

		val expected = new Value(VarType.Bool, Option("False"))

		assert(res.isSame(expected))
	}

	test("compute: 3-2+5*4;") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("3", LexemeType.IntNumber, 1))
			.add(new Lexeme("-", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("2", LexemeType.IntNumber, 1))
			.add(new Lexeme("+", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("5", LexemeType.IntNumber, 1))
			.add(new Lexeme("*", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("4", LexemeType.IntNumber, 1))
			.add(new Lexeme(";", LexemeType.Semicolon, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		val res = lexemesParser.compute(new VarTable)

		val expected = new Value(VarType.Int, Option("21"))

		assert(res.isSame(expected))
	}

	test("compute: 50-(2+5)*4;") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("50", LexemeType.IntNumber, 1))
			.add(new Lexeme("-", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("(", LexemeType.Brackets, 1))
			.add(new Lexeme("2", LexemeType.IntNumber, 1))
			.add(new Lexeme("+", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("5", LexemeType.IntNumber, 1))
			.add(new Lexeme(")", LexemeType.Brackets, 1))
			.add(new Lexeme("*", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("4", LexemeType.IntNumber, 1))
			.add(new Lexeme(";", LexemeType.Semicolon, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		val res = lexemesParser.compute(new VarTable)

		val expected = new Value(VarType.Int, Option("22"))

		assert(res.isSame(expected))
	}

	test("compute: 10 % (1+2*(1+1)*1) == 2*2-4;") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("10", LexemeType.IntNumber, 1))
			.add(new Lexeme("%", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("(", LexemeType.Brackets, 1))
			.add(new Lexeme("1", LexemeType.IntNumber, 1))
			.add(new Lexeme("+", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("2", LexemeType.IntNumber, 1))
			.add(new Lexeme("*", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("(", LexemeType.Brackets, 1))
			.add(new Lexeme("1", LexemeType.IntNumber, 1))
			.add(new Lexeme("+", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("1", LexemeType.IntNumber, 1))
			.add(new Lexeme(")", LexemeType.Brackets, 1))
			.add(new Lexeme("*", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("1", LexemeType.IntNumber, 1))
			.add(new Lexeme(")", LexemeType.Brackets, 1))
			.add(new Lexeme("==", LexemeType.BoolOp, 1))
			.add(new Lexeme("2", LexemeType.IntNumber, 1))
			.add(new Lexeme("*", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("2", LexemeType.IntNumber, 1))
			.add(new Lexeme("-", LexemeType.ArithmeticOp, 1))
			.add(new Lexeme("4", LexemeType.IntNumber, 1))
			.add(new Lexeme(";", LexemeType.Semicolon, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		val res = lexemesParser.compute(new VarTable)

		val expected = new Value(VarType.Bool, Option("True"))

		assert(res.isSame(expected))
	}

	test("change global var") {
		val program = "x = 3;\n" +
			"y = False or True;\n" +
			"def main(): None {\n" +
			"x = 5;\n" +
			"y = False and True;\n" +
			"}"

		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse().run()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar1: Value = new Value(VarType.Int, Option("5"))
		val expectedVar2: Value = new Value(VarType.Bool, Option("False"))
		expected.setVal("x", expectedVar1)
		expected.setVal("y", expectedVar2)

		assert(sameVarTables(expected, globalVars))
	}

	test("change global vars with vars") {
		val program = "x = 3;\n" +
			"def main(): None {\n" +
			"y = 5;\n" +
			"x = x+y*x;\n" +
			"}"

		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse().run()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar1: Value = new Value(VarType.Int, Option("18"))
		expected.setVal("x", expectedVar1)

		assert(sameVarTables(expected, globalVars))
	}

	test("change global var in if") {
		val program = "x = 0;\n" +
									"def main(): None {\n" +
										"if (0 == 1;) {x = 1;}\n" +
										"else {\n" +
											"if (1+1 <= 3;) {x = 2;}\n" +
											"else {x=3;}\n" +
										"}" +
									"}"

		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse().run()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar1: Value = new Value(VarType.Int, Option("2"))
		expected.setVal("x", expectedVar1)

		assert(sameVarTables(expected, globalVars))
	}

	test("change global var in while") {
		val program = "x = 0;\n" +
									"def main(): None {\n" +
										"i = 5;\n" +
										"while (i > x;) {x = x + 1;}\n" +
									"}"

		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse().run()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar1: Value = new Value(VarType.Int, Option("5"))
		expected.setVal("x", expectedVar1)

		assert(sameVarTables(expected, globalVars))
	}

	test("change global var in while and if") {
		val program = "x = 0;\n" +
									"def main(): None {\n" +
										"i = 10;\n" +
										"while (i > 0;) {\n" +
											"i = i - 1;\n" +
											"if (x % 2 == 0;) {\n" +
												"x = x + 1;\n" +
											"} else {\n" +
												"x = x + 3;\n" +
											"}\n" +
										"}\n" +
									"}"

		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse().run()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar1: Value = new Value(VarType.Int, Option("20"))
		expected.setVal("x", expectedVar1)

		assert(sameVarTables(expected, globalVars))
	}

	test("change global var in 2 while structures") {
		val program = "x = 100;\n" +
									"y = 1;\n" +
									"z = 50;\n" +
									"def main(): None {\n" +
										"while (x > 0;) {\n" +
											"while (z > 0;) {\n" +
												"y = y + 3;\n" +
												"z = z - 17;\n" +
											"}\n" +
											"z = x;\n" +
											"x = x - 20;\n" +
										"}\n" +
									"}"

		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse().run()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVarX: Value = new Value(VarType.Int, Option("0"))
		val expectedVarY: Value = new Value(VarType.Int, Option("64"))
		val expectedVarZ: Value = new Value(VarType.Int, Option("20"))
		expected.setVal("x", expectedVarX)
		expected.setVal("y", expectedVarY)
		expected.setVal("z", expectedVarZ)

		assert(sameVarTables(expected, globalVars))
	}

	test("change global var in for loop") {
		val program = "x = 3;\n" +
									"def main(): None {\n" +
										"for (i from 0; to 10; step 2;) {\n" +
											"x = x + i;" +
										"}\n" +
									"}"

		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse().run()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Int, Option("23"))
		expected.setVal("x", expectedVar)

		assert(sameVarTables(expected, globalVars))
	}

	test("change global var in 2 for loops") {
		val program = "x = 0;\n" +
									"def main(): None {\n" +
										"for (i from 0; to 10;) {\n" +
											"for (j from 0; to 10;) {\n" +
												"x = x + 1;" +
											"}\n" +
										"}\n" +
									"}"

		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse().run()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Int, Option("100"))
		expected.setVal("x", expectedVar)

		assert(sameVarTables(expected, globalVars))
	}

	test("change global var in function") {
		val program = "x = 1;\n" +
									"def f1(p: Int): Int {\n" +
										"return x + p * 2;\n" +
									"}\n" +
									"def main(): None {\n" +
										"x = f1(5+5;) + f1(x)\n" +
									"}"

		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse().run()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Int, Option("24"))
		expected.setVal("x", expectedVar)

		assert(sameVarTables(expected, globalVars))
	}

	test("change global var in function with no parameters") {
		val program = "x = 1;\n" +
									"def xMult2(): None {\n" +
										"x = x * 2;\n" +
									"}\n" +
									"def xMult3(): None {\n" +
										"x = x * 3;\n" +
									"}\n" +
									"def xMult5(): None {\n" +
										"x = x * 5;\n" +
									"}\n" +
									"def main(): None {\n" +
										"xMult2()\n" +
										"xMult3()\n" +
										"xMult2()\n" +
										"xMult5()\n" +
									"}"

		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse().run()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Int, Option("60"))
		expected.setVal("x", expectedVar)

		assert(sameVarTables(expected, globalVars))
	}

	test("change global var in function with parameters") {
		val program = "x = 1;\n" +
									"def xMultN(n: Int): None {\n" +
										"x = x * n;\n" +
									"}\n" +
									"def xMultNPlusM(n: Int, m: Int): None {\n" +
										"x = x * n + m;\n" +
									"}\n" +
									"def main(): None {\n" +
										"xMultN(10;)\n" +
										"xMultNPlusM(1+1;, 7;)\n" +
										"xMultN(x;)\n" +
									"}"

		val lexicalAnalyzer = new LexicalAnalyzer
		lexicalAnalyzer.run(program)

		val lexemesParser = new LexemesParser(lexicalAnalyzer.lexemesTable)
		lexemesParser.parse().run()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar: Value = new Value(VarType.Int, Option("729"))
		expected.setVal("x", expectedVar)

		assert(sameVarTables(expected, globalVars))
	}
}