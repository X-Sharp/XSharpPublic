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
        /*
        DbCreate("Test",{{"TEST","C",10,0},{"TEST2","C",10,0}})
        DbUseArea(TRUE,,"TEST")
        DbCreateIndex("test","UPPER(Test)")
        DbCreateIndex("test2","UPPER(Test2)")
        DbCloseArea()
        */        
        ReportMemory("initial")
        VAR mc := CreateMacroCompiler()
        VAR fmc := XSharp.Runtime.MacroCompiler{XSharp.MacroCompiler.MacroOptions.FoxPro}

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

        ParserTestsFox(fmc)
        ParserTests(mc)
        VoTests(mc)
        FoxTests(fmc)
        //wait

        RunPerf(mc, "Console.WriteLine(123)")

        ReportMemory("final");

        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()

END NAMESPACE

