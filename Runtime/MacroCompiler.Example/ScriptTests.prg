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
        "#pragma options(""az"", on)",;
        "return 10",;
        ""}),Args(), null, null,   ErrorCode.ERR_PragmaNotSupported)

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
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "LPARAMETERS a, b, c",;
        "RETURN A+B"}), Args(1,2), 3, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "PARAMETERS a, b, c",;
        "RETURN a+b+c"}),Args(1,2,3), 6, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        e"THROW Exception{\"Hello\"}",;
        "RETURN 1"}),Args(), "Hello", typeof(Exception))
    TestMacro(sc, String.Join(e"\n",<STRING>{e"THROW 0"}), Args(), null, null, ErrorCode.TypeMustDeriveFrom)
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "TRY",;
        "CATCH a AS INT",;
        "END"}), Args(), null, null, ErrorCode.TypeMustDeriveFrom)
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "TRY",;
        e"THROW Exception{\"Hello\"}",;
        "END",;
        "RETURN 321"}), Args(), 321, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 0",;
        "TRY",;
        "FINALLY",;
        "x := 123",;
        "END",;
        "RETURN x"}), Args(), 123, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 0",;
        "TRY",;
        e"THROW Exception{\"Hello\"}",;
        "CATCH",;
        "FINALLY",;
        "x := 123",;
        "END",;
        "RETURN x"}), Args(), 123, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "x := 0",;
        "TRY",;
        e"THROW Exception{\"Hello\"}",;
        "CATCH e AS Exception",;
        "x := e:Message",;
        "END",;
        "RETURN x"}), Args(), "Hello", typeof(string))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "TRY",;
        e"BREAK 123",;
        "CATCH e AS XSharp.Internal.WrappedException",;
        "x := e:Value",;
        "END",;
        "RETURN x"}), Args(), 123, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "TRY",;
        e"THROW Exception{\"Hello\"}",;
        "RETURN 0",;
        "CATCH e AS Exception",;
        "RETURN e:Message",;
        "CATCH e AS XSharp.Internal.WrappedException",;
        "FINALLY",;
        "END"}), Args(), "Hello", typeof(string))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "TRY",;
        "FINALLY",;
        "RETURN 0",;
        "END"}), Args(), null, null, ErrorCode.ReturnNotAllowed)
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "TRY",;
        "RETURN 123",;
        "FINALLY",;
        "END"}), Args(), 123, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "BEGIN SEQUENCE",;
        "BREAK 123",;
        "RECOVER USING e",;
        "RETURN e",;
        "FINALLY",;
        "END SEQUENCE"}), Args(), 123, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "IF true",;
        "RETURN 1",;
        "ENDIF"}), Args(), 1, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "var o := object{}",;
        "BEGIN LOCK o",;
        "o := 1",;
        "END LOCK",;
        "RETURN o"}), Args(), 1, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "LPARAMETERS s",;
        "var reader := System.IO.StringReader{s}",;
        "begin using reader",;
        "VAR item := reader:ReadLine()",;
        "end using",;
        "reader:ReadLine()"}),Args("hello"), "Cannot read from a closed TextReader.", typeof(System.ObjectDisposedException))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "PARAMETERS a, b, c",;
        "VAR CONST x := a+b+c",;
        "RETURN x"}), Args(1,2,3), null, null, ErrorCode.ValueNotConst)
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "VAR CONST x := 1+2+3",;
        "RETURN x"}), Args(), 6, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "VAR CONST x := 1+2+3",;
        "x := 5"}), Args(), null, null, ErrorCode.NoAccessMode)
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "LOCAL CONST x := 3+2+1 as int",;
        "RETURN x"}), Args(), 6, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "LOCAL CONST x as int",;
        "RETURN x"}), Args(), null, null, ErrorCode.ConstWithoutInitializer)
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "LOCAL DIM a[10] as INT",;
        "RETURN a[1]"}), Args(), 0, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "LOCAL DIM a[10,10] as INT",;
        "RETURN a[1,1]"}), Args(), 0, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "local a[10]",;
        "a[10] := 123",;
        "RETURN a[1]"}),Args(), null, null)
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "local a[10,10]",;
        "a[10,10] := 123",;
        "RETURN a[1,1]"}),Args(), null, null)
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "VAR v := 0",;
        "IncrInt(v)",;
        "RETURN v"}), Args(), 1, typeof(INT))

    Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGet, "MyFieldGet")
    Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSet, "MyFieldSet")
    Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGetWa, "MyFieldGetWa")
    Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSetWa, "MyFieldSetWa")
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "FIELD a",;
        "RETURN a"}),Args(), "FieldGet(a)", typeof(string))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "FIELD a",;
        "a:=123"}),Args(), 123, typeof(int))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "FIELD a IN db",;
        "RETURN a"}),Args(), "FieldGet(db,a)", typeof(string))
    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "FIELD a IN db",;
        "a:=123"}),Args(), 123, typeof(int))

    RETURN

FUNCTION TestPreProcessor(sc AS XSharp.Runtime.MacroCompiler) AS VOID
    TestMacro(sc, String.Join(e"\n",<STRING>{;
    "#include ""XSharpDefs.xh"" ",;
    "PARAMETERS a, b, c",;
    "#define AAA",;
    "#ifdef AAA",;
    "RETURN 0",;
    "#endif",;
    "RETURN a+b+c"}),Args(1,2,3), 0, typeof(INT))

    RETURN

FUNCTION TestUDC(sc AS XSharp.Runtime.MacroCompiler) AS VOID
    // Create test data file for the UDC test
    DbCreate("Test",{{"TEST1","C",10,0},{"TEST2","C",10,0}})
    DbUseArea(TRUE,,"TEST")
    FOR VAR i := 1 TO 10
        DbAppend()
        FieldPut(1, StrZero(i,10,0))
        FieldPut(2, Repl(Chr(64+(DWORD)i),10))
    NEXT
    DbCreateIndex("test1","UPPER(Test1)")
    DbCreateIndex("test2","UPPER(Test2)")
    DbCloseArea()


    TestMacro(sc, String.Join(e"\n",<STRING>{;
    "#include ""XSharpDefs.xh"" ",;
    "USE TEST",;
    "i:=1",;
    "DO WHILE ! EOF()",;
    " IF FieldGet(1) != StrZero(i,10,0)",;
    "  RETURN false",;
    " END",;
    " IF FieldGet(2) != Repl(Chr(64+(DWORD)i),10)",;
    "  RETURN false",;
    " END",;
    " i++",;
    " SKIP ",;
    "ENDDO",;
    "CLOSE",;
    "RETURN true"}), Args(1), TRUE, typeof(LOGIC))

    XSharp.RuntimeState.Dialect := XSharpDialect.FoxPro
    sc := CreateFoxScriptCompiler()
    TestMacro(sc, String.Join(e"\n",<STRING>{;
    "#include ""XSharpDefs.xh"" ",;
    "LPARAMETERS fileName",;
    "LOCAL recCount",;
    "USE (fileName)",;
    "i:=1",;
    "SCAN",;
    " IF Test->Test1 != StrZero(i,10,0)",;
    "  RETURN false",;
    " END",;
    " IF Test->Test2 != Repl(Chr(64+(DWORD)i),10)",;
    "  RETURN false",;
    " END",;
    " i++",;
    "ENDSCAN",;
    "recCount = LastRec()",;
    "CLOSE ",;
    "RETURN true"}),Args("test"), TRUE, typeof(LOGIC))

    TestMacro(sc, String.Join(e"\n",<STRING>{;
        "#include ""XSharpDefs.xh"" ",;
        "x := ''",;
        "TEXT TO x",;
        "aaa",;
        "bbb",;
        "ccc",;
        "ENDTEXT",;
        "return x",;
        ""}),Args(), e"aaa\r\nbbb\r\nccc\r\n", typeof(string))

    RETURN

