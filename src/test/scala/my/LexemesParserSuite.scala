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
		val expectedVar: Variable = new Variable("idx", VarType.Int, Option("3"))
		expected.setVal(expectedVar)

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
		val expectedVar: Variable = new Variable("x", VarType.Double, Option("3"))
		expected.setVal(expectedVar)

		assert(sameVarTables(expected, globalVars))
	}

	test("idx: Boolean = 3") {
		val lexemeTable: LexemeTable = new LexemeTable()
			.add(new Lexeme("idx", LexemeType.Name, 1))
			.add(new Lexeme(":", LexemeType.Colon, 1))
			.add(new Lexeme("Boolean", LexemeType.Type, 1))
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
		val expectedVar: Variable = new Variable("s", VarType.String, None)
		expected.setVal(expectedVar)

		assert(sameVarTables(expected, globalVars))
	}
}