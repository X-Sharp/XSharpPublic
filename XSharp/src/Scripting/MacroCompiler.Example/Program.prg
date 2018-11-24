using System
using System.Collections.Generic
using System.Linq
using System.Text

function U(u as usual) as usual
    return u

function Test(u as usual) as void
    Console.WriteLine("Test: {0}", u)
    return

function TestR(u as real8) as void
    Console.WriteLine("TestR: {0:##.#}", u)
    return

function TestI(u as int) as void
    Console.WriteLine("TestI: {0}", u)
    return

global UU as usual

begin namespace MacroCompilerTest
    using XSharp.Runtime
    using XSharp.MacroCompiler

	function Start() as void
	    SetMacroCompiler(typeof(XSharp.Runtime.MacroCompiler))

        //var test_source := "Console.WriteLine(123)";
        //var test_source := "Test(12345)";
        //var test_source := "Test(U(12345)-1)";
        //var test_source := "Test(123+45)";
        //var test_source := "TestR(123)";
        //var test_source := "TestI(123.456)";
        //var test_source := "{|a,b,c|a := b := 1343+1}";
        //var test_source := "{|a,b,c|}";
        //var test_source := "{|a,b,c|1234}";
        //var test_source := "U(1234)";
        //var test_source := "1234";
        //var test_source := "";
        //var test_source := "{|a,b,c|a := b := 1343, c := a + 1, a+b-c/2}";
        //var test_source := "{|a|a := 1343, a += 1}";
        //var test_source := "{|a|a := -1343, a := -a}";
        //var test_source := "{|a|a := 8, a := 8**a}";
        //var test_source := "{|a|a := 8, ++a, ++a}";
        //var test_source := "{|a| a:= 8, ++a, a++, a++}";
        //var test_source := "{|a| ++a, a++, a++}";
        //var test_source := "{|a| a++, Console.WriteLine(123), a++}";
        //var test_source := e"{|a| a := \"abc\", Console.WriteLine(\"Hello\"), a + \"def\" }";
        //var test_source := e"{|a| \"abc\" + \"def\"}";
        //var test_source := e"{|a| \"abc\" == \"def\"}";
        //var test_source := e"{|a| \"abc\" = \"abc\"}";
        //var test_source := e"{|a| \"abc\" != \"abc\"}";
        //var test_source := e"{|a| a := \"abc\", a == \"abc\"}";
        //var test_source := e"{|a| 0 == 0 }";
        //var test_source := e"{|a| 0 != 0 }";
        //var test_source := e"{|a| (0 > 1) && (0 < 1) }";
        //var test_source := e"{|a| a:ToString() }";
        //var test_source := e"{|a| a := \"qwerty\", a:Length }";
        //var test_source := e"{|a| a := default(int) }";
        //var test_source := e"{|a| a := default(string) }";
        //var test_source := e"{|a| a := U(1234+1), a }";
        var test_source := e"{|a| UU := U(1234+1), UU }";

        ReportMemory("initial")
        var mc := CreateMacroCompiler()

        begin scope
            Console.WriteLine("Executing macro ...")
            var cb := mc:Compile(test_source)
            var res := cb:EvalBlock(8)
            Console.WriteLine("res = {0}",res)
wait
//return
        end scope

        TestMacroCompiler(mc, test_source, 15, true, false)
        TestMacroCompiler(mc, test_source, 15, true, true)
        TestMacroCompiler(mc, test_source, 100000, false, false)
        TestMacroCompiler(mc, test_source, 100000, false, true)

        ReportMemory("final");

        Console.WriteLine("Press any key to continue...")
        Console.ReadKey()

    function CreateMacroCompiler() as XSharp.Runtime.MacroCompiler
        Console.WriteLine("Creating macro compiler ...")

        var m := GC:GetTotalMemory(true)
        var t := DateTime.Now

        var mc := XSharp.Runtime.MacroCompiler{}

        var dt := DateTime.Now - t
        t += dt

        Console.WriteLine("  Completed in {0}", dt)
        Console.WriteLine("  Memory: +{0} bytes", GC.GetTotalMemory(false) - m)
        Console.WriteLine()
        return mc

    function ReportMemory(description as string) as void
        Console.WriteLine("Memory: {0} ({1})", GC:GetTotalMemory(true),description)
        Console.WriteLine()
        return

    function TestMacroCompiler(mc as XSharp.Runtime.MacroCompiler, source as string, iterations as int, check_mem as logic, compile as logic) as void
        Console.WriteLine("Start {0} {1} ({2} iterations) ...", iif(compile,"compiler","parser"), iif(check_mem,"memory test","benchmark"), iterations);

        var m := GC:GetTotalMemory(true)
        var t := DateTime.Now

        for var i := 0 to iterations
            local m0 := 0 as int64
            if (check_mem)
                m0 := GC:GetTotalMemory(FALSE)
            end

            if (compile)
                var o := MCompile(source)
                //var o := mc:Compile(source)
                //o:Eval()
            else
                var ast := mc:compiler:Parse(source)
                //Console.WriteLine(ast);
            end

            if (check_mem)
                Console.WriteLine("  Iteration {0} memory: +{1} bytes", i+1, GC:GetTotalMemory(false) - m0)
            end
        next

        var dt := DateTime.Now - t
        t += dt
        if (!check_mem)
            Console.WriteLine("  Completed in {0} ({1} ms/iter, {2:#} iters/sec)", dt, dt:TotalMilliseconds/iterations, iterations/dt:TotalSeconds)
            Console.WriteLine("  Memory: +{0} bytes", GC:GetTotalMemory(false) - m)
        end

        Console.WriteLine()
        return

end namespace

begin namespace XSharp.Runtime
    using XSharp.MacroCompiler
    //using Vulcan.Runtime
    using XSharp

    delegate RuntimeCodeblockDelegate(args params dynamic[]) as dynamic

    public class RuntimeCodeblock implements ICodeBlock
        private _eval as RuntimeCodeblockDelegate
        private _pcount as int

        public method EvalBlock(args params dynamic[]) as dynamic
            return _eval(args)

        public method PCount() as int
            return _pcount

        public constructor(evalMethod as RuntimeCodeblockDelegate, pCount as int)
            _eval := evalMethod
            _pcount := pCount
    end class

    public class MacroCompiler implements IMacroCompiler
        internal compiler := Compilation.Create<object,RuntimeCodeblockDelegate>() as Compilation<object,RuntimeCodeblockDelegate>

	    public method Compile (cMacro as string, lOldStyle as logic, Module as System.Reflection.Module, lIsBlock ref logic) as ICodeBlock
		    lIsBlock := cMacro:StartsWith("{|")
            var m := compiler:Compile(cMacro)
    	    return RuntimeCodeblock{m:Macro,m:ParamCount}

	    public method Compile (cMacro as string) as ICodeBlock
            var m := compiler:Compile(cMacro)
    	    return RuntimeCodeblock{m:Macro,m:ParamCount}
    end class

end namespace
