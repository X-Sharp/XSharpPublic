// ScriptTests.prg
// Created by    : neek
// Creation Date : 3/6/2021 8:17:14 PM
// Created for   : 
// WorkStation   : I7


USING System
USING System.Collections.Generic
USING System.Text
USING System.Linq
USING XSharp.Runtime
USING XSharp.MacroCompiler

FUNCTION ScriptTests AS VOID
    var sc := CreateScriptCompiler()

    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "Console.WriteLine(123)",;
        "NOP",;
        "NOP()",;
        "123",;
        ""}),Args(), 123, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 1",;
        "IF x == 1",;
          "return true",;
        "ELSE",;
          "return false",;
        "END",;
        ""}),Args(), true, typeof(logic))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := ;","10",;
        "DO CASE",;
        "CASE x == 0",;
          "return true",;
        "case x == 1",;
          "return true",;
        "otherwise",;
          "return x",;
        "END CASE",;
        "return -1",;
        ""}),Args(), 10, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 1",;
        "WHILE x <= 10",;
          "x += 1",;
        "END",;
        "x"}),Args(), 11, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 1",;
        "REPEAT",;
          "x += 1",;
        "UNTIL x > 10",;
        "x"}),Args(), 11, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "y := 0",;
        "FOR x := 1 TO 10",;
            "y += 1",;
        "END FOR",;
        "FOR x := 1 UPTO 10",;
            "y += 1",;
        "NEXT",;
        "FOR x := 10 DOWNTO 1",;
            "y += 1",;
        "NEXT",;
        "FOR x := 1 UPTO 20 STEP 2",;
            "y += 1",;
        "NEXT",;
        "FOR x := 20 DOWNTO 1 STEP 2",;
            "y += 1",;
        "NEXT",;
        "FOR x := 1 TO 10 step -1",;
            "y += 1",;
        "NEXT",;
        "FOR x := 1 TO 40 step 4",;
            "y += 1",;
        "NEXT",;
        "y"}),Args(), 60, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "y := 0",;
        "FOR VAR l := 1 TO 40 step 4",;
            "y += 1",;
        "NEXT",;
        "FOR LOCAL l := 1 AS INT TO 40 step 4",;
            "y += 1",;
        "NEXT",;
        "FOR LOCAL IMPLIED l3 := 1 TO 40 step 4",;
            "y += 1",;
        "NEXT",;
        "y"}),Args(), 30, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "y := 0",;
        "BEGIN SCOPE",;
            "y += 1",;
            "x := y",;
        "END SCOPE",;
        "x"}),Args(), 1, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 0, y := 0",;
        "FOR VAR l := 1 TO 40",;
            "x += 1",;
            "LOOP",;
            "y += 1",;
        "NEXT",;
        "x+y"}),Args(), 40, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 10",;
        "WHILE --x > 0",;
            "LOOP",;
            "x := -100",;
        "END",;
        "x"}),Args(), 0, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 10",;
        "REPEAT",;
            "LOOP",;
            "x := -100",;
        "UNTIL --x <= 0",;
        "x"}),Args(), 0, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "y := 0",;
        "FOR VAR l := 1 TO 40",;
            "y += 1",;
            "EXIT",;
        "NEXT",;
        "y"}),Args(), 1, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 10",;
        "WHILE --x > 0",;
            "x := 100",;
            "EXIT",;
        "END",;
        "x"}),Args(), 100, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 10",;
        "REPEAT",;
            "x := 100",;
            "EXIT",;
        "UNTIL --x <= 0",;
        "x"}),Args(), 100, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{"EXIT"}),Args(), NULL, NULL, ErrorCode.NoExitableStatement)
    TestMacro(sc, String.Join(e"\n",<STRING>{"LOOP"}),Args(), NULL, NULL, ErrorCode.NoLoopableStatement)
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "?",;
        "? 1+1, 10, 100",;
        "?? 5, 6, 7",;
        "??"}),Args(), null, null)
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 123",;
        "DO SWITCH x",;
        "CASE k as USUAL",;
            "x := 1",;
        "CASE 1",;
            "x := 1",;
        "CASE 123",;
        "CASE 1234",;
        "CASE 12345",;
            "x := 0",;
        "OTHERWISE",;
            "x := -1",;
        "END",;
        "x"}),Args(), 1, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "y := 0",;
        "FOR VAR l := 1 TO 5",;
            "y += testglobals.tsa[l]",;
        "NEXT",;
        "y"}),Args(), 165, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 123",;
        "DO SWITCH x",;
        "OTHERWISE",;
        "OTHERWISE",;
            "RETURN 1",;
        "END",;
        "x"}),Args(), null, null, ErrorCode.MultipleDefaultInSwitch)
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "y := 0",;
        "return 1",;
        "y"}),Args(), 1, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "y := 0",;
        "FOREACH VAR l IN testglobals.tsa",;
            "y += l",;
        "NEXT",;
        "y"}),Args(), 165, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "y := 0",;
        "list := {1,2,3,4,5}",;
        "FOREACH VAR l IN list",;
            "y += l",;
        "NEXT",;
        "y"}),Args(), 15, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "LOCAL y := 0 AS INT",;
        "list := {1,2,3,4,5}",;
        "FOREACH VAR l IN list",;
            "y += l",;
        "NEXT",;
        "y"}), Args(), 15, typeof(int))
    EvalMacro(sc, String.Join(e"\n",<STRING>{;
        "VAR y := 0",;
        "list := {1,2,3,4,5}",;
        "FOREACH VAR l IN list",;
            "y += l",;
        "NEXT",;
        "y"}), Args(), 15, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "LPARAMETERS a, b, c",;
        "RETURN A+B+C"}), Args(1,2,3), 6, typeof(int))

    RETURN
