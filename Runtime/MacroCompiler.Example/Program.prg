USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharp.Runtime
USING XSharp.MacroCompiler

BEGIN NAMESPACE MacroCompilerTest

    FUNCTION Start() AS VOID
        SetMacroCompiler(typeof(XSharp.Runtime.MacroCompiler))
        // test conflict between field name and global/define

        ReportMemory("initial")
        VAR mc := CreateMacroCompiler()
        VAR fmc := CreateFoxMacroCompiler()

        //EvalMacro(mc, "{|| 0000.00.00 }" ,NULL_DATE)
        //ParseMacro(mc, e"{|a,b| +a[++b] += 100, a[2]}")
        //EvalMacro(mc, e"{|a,b| a[++b] += 100, a[2]}", {1,2,3}, 1)
        //EvalMacro(mc, e"{|a|A,1_000", 123)
        //EvalMacro(mc, e"{|a| USUAL(-a) }", 1)
        //EvalMacro(mc, e"{|| testclass{}:NString((byte)1) }", Args())
        //EvalMacro(mc, e"{|a,b| b := testclass{123}, b:ToString() }")
        //EvalMacro(mc, e"0.00001")
        //EvalMacro(mc, "{|foo| bar := 10}")
        //EvalMacro(mc, "{|foo| bar := 10,foo}")
        //wait
        //EvalMacro(mc, "{|| 1+(2+3))))}")
        //EvalMacro(mc, "1+(2+3)))")
        //EvalMacro(mc, "{ || NIL } ")
        //wait

        /*var sc := CreateScriptCompiler()
        EvalMacro(sc, String.Join(e"\n",<STRING>{;
        "PARAMETERS a, b, c",;
        "RETURN a+b+c"}),1,2,3)
        wait*/

        VAR sc := CreateScriptCompiler()
        EvalMacro(sc, String.Join(e"\n",<STRING>{;
        "#include ""XSharpDefs.xh"" ",;
        "PARAMETERS a, b, c",;
        "#define AAA",;
        "#ifdef AAA",;
        "RETURN 0",;
        "#endif",;
        "RETURN a+b+c"}),1,2,3)
        wait

        testUDC(sc)


        ParserTestsFox(CreateFoxScriptCompiler())
        ParserTests(CreateScriptCompiler())
        ScriptTests()
        VoTests(mc)
        FoxTests(fmc)

        RunPerf(mc, "Console.WriteLine(123)")

        ReportMemory("final");

        Console.WriteLine("Total pass: {0}/{1}", TotalSuccess, TotalTests)
        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()

    END NAMESPACE


FUNCTION TestUDC(sc AS XSharp.Runtime.MacroCompiler) AS VOID
    // Create test data file for the UDC test
    DbCreate("Test",{{"TEST","C",10,0},{"TEST2","C",10,0}})
    DbUseArea(TRUE,,"TEST")
    FOR VAR i := 1 TO 100
        DbAppend()
        FieldPut(1, StrZero(i,10,0))
        FieldPut(2, Repl(Chr(64+(DWORD)i),10))
    NEXT
    DbCreateIndex("test","UPPER(Test)")
    DbCreateIndex("test2","UPPER(Test2)")
    DbCloseArea()


    EvalMacro(sc, String.Join(e"\n",<STRING>{;
    "#include ""XSharpDefs.xh"" ",;
    "USE TEST",;
    "DO WHILE ! EOF()",;
    " ? FieldGet(1), FieldGet(2)",;
    " SKIP ",;
    "ENDDO",;
    "CLOSE",;
    "RETURN NIL"}),1)
    WAIT
    RETURN
