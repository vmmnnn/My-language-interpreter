package my

import org.scalatest.FunSuite


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

	test("idx = 3") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("idx", LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))
			.add(new Lexeme("3", LexemeType.IntNumber, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		lexemesParser.run()

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
		lexemesParser.run()

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
		intercept[Exception] { lexemesParser.run() }
	}

	test("s: String") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("s", LexemeType.Name, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))
			.add(new Lexeme("String", LexemeType.Type, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		lexemesParser.run()

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
		intercept[Exception] { lexemesParser.run() }
	}

	test("x = ") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("x", LexemeType.Name, 1))
			.add(new Lexeme("=", LexemeType.DefineOp, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		intercept[Exception] { lexemesParser.run() }
	}

	test("x : ") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("x", LexemeType.Name, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		intercept[Exception] { lexemesParser.run() }
	}

	test("x") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("x", LexemeType.Name, 1))

		val lexemesParser = new LexemesParser(lexemeTable)
		intercept[Exception] { lexemesParser.run() }
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
		lexemesParser.run()

		val globalVars = lexemesParser.getGlobalVars

		val expected: VarTable = new VarTable
		val expectedVar1: Value = new Value(VarType.Int, Option("25"))
		val expectedVar2: Value = new Value(VarType.Bool, Option("True"))
		expected.setVal("x", expectedVar1)
		expected.setVal("fl", expectedVar2)

		globalVars.print()

		assert(sameVarTables(expected, globalVars))
	}

}