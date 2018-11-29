using System
using System.Collections.Generic
using System.Linq
using System.Text

function U(u as usual) as usual
    return u

function R(r as real8) as real8
    return r

function I(i as int) as int
    return i

function A(i ref int) as int
    return i;

function I0() as int
    return 123;

function I3(a := 1 as int, b := 2 as int, c := 3 as int)
    return a+b+c

function CC(a,b,c)
    if a == NIL
        a := 0
    end
    if b == NIL
        b := 0
    end
    if c == NIL
        c := 0
    end
    return a+b+c

global UU as usual

begin namespace MacroCompilerTest
    using XSharp.Runtime
    using XSharp.MacroCompiler

	function Start() as void
	    SetMacroCompiler(typeof(XSharp.Runtime.MacroCompiler))

        ReportMemory("initial")
        var mc := CreateMacroCompiler()

        EvalMacro(mc, e"{|a| a := {1,,2,,} }")
        wait

        RunTests(mc)
        wait

        RunPerf(mc, "Console.WriteLine(123)")

        ReportMemory("final");

        Console.WriteLine("Press any key to continue...")
        Console.ReadKey()

    function EvalMacro(mc as XSharp.Runtime.MacroCompiler, src as string) as void
        Console.WriteLine("Executing macro ...")
        //var cb := MCompile(src)
        var cb := mc:Compile(src)
        var res := cb:EvalBlock(8)
        Console.WriteLine("res = {0}",res)
        return

    global TotalFails := 0 as int
    global TotalTests := 0 as int
    global TotalSuccess := 0 as int

    function RunTests(mc as XSharp.Runtime.MacroCompiler) as void
        Console.WriteLine("Running tests ...")

        TestMacro(mc, "U(12345)", <OBJECT>{}, 12345, typeof(usual))
        TestMacro(mc, "U(U(12345)-1)", <OBJECT>{}, 12344, typeof(usual))
        TestMacro(mc, "I(123+45)", <OBJECT>{}, 123+45, typeof(int))
        TestMacro(mc, "R(123)", <OBJECT>{}, 123, typeof(real8))
        TestMacro(mc, "R(123.456)", <OBJECT>{}, 123.456, typeof(real8))
        TestMacro(mc, "U(123.456)", <OBJECT>{}, 123.456, typeof(usual))
        TestMacro(mc, "{|a,b,c|a := b := 1343+1}", <OBJECT>{}, 1343+1, typeof(int))
        TestMacro(mc, "{|a,b,c|}", <OBJECT>{}, null, null)
        TestMacro(mc, "{|a,b,c|1234}", <OBJECT>{}, 1234, typeof(int))
        TestMacro(mc, "1234", <OBJECT>{}, 1234, typeof(int))
        TestMacro(mc, "", <OBJECT>{}, null, null)
        TestMacro(mc, "{|a,b,c|a := b := 1343, c := a + 1, a+b-c/2}", <OBJECT>{}, 1343+1343-(1343+1)/2, typeof(usual))
        TestMacro(mc, "{|a|a := 1343, a += 1}", <OBJECT>{}, 1343+1, typeof(usual))
        TestMacro(mc, "{|a|a := -1343, a := -a}", <OBJECT>{}, 1343, typeof(usual))
        TestMacro(mc, "{|a|a := 8, ++a, ++a}", <OBJECT>{123}, 10, typeof(usual))
        TestMacro(mc, "{|a|a := 8, ++a, a++, a++}", <OBJECT>{123}, 10, typeof(usual))
        TestMacro(mc, "{|a|++a, a++, a++}", <OBJECT>{8}, 10, typeof(usual))
        TestMacro(mc, "{|a| a++, U(a++), a++}", <OBJECT>{8}, 10, typeof(usual))
        TestMacro(mc, e"{|a| a := \"abc\" + \"def\"}", <OBJECT>{8}, "abcdef", typeof(string))
        TestMacro(mc, e"{|a| \"abc\" == \"def\"}", <OBJECT>{8}, false, typeof(logic))
        TestMacro(mc, e"{|a| \"abc\" = \"abc\"}", <OBJECT>{8}, true, typeof(logic))
        TestMacro(mc, e"{|a| \"abc\" != \"abc\"}", <OBJECT>{8}, false, typeof(logic))
        TestMacro(mc, e"{|a| a := \"abc\", a == \"abc\"}", <OBJECT>{8}, true, typeof(logic))
        TestMacro(mc, e"{|a| a := \"abc\", a + \"def\"}", <OBJECT>{8}, "abcdef", typeof(string))
        TestMacro(mc, e"{|a| 0 == 0}", <OBJECT>{8}, true, typeof(logic))
        TestMacro(mc, e"{|a| 0 != 0}", <OBJECT>{8}, false, typeof(logic))
        TestMacro(mc, e"{|a| (0 > 1) && (0 < 1) }", <OBJECT>{8}, false, typeof(logic))
        TestMacro(mc, e"{|a| a := \"qwerty\", a:Length }", <OBJECT>{8}, 6, typeof(usual))
        TestMacro(mc, e"{|a| a := default(int) }", <OBJECT>{8}, 0, typeof(int))
        TestMacro(mc, e"{|a| a := default(string) }", <OBJECT>{8}, null, null)
        TestMacro(mc, e"{|a| a := default(usual) }", <OBJECT>{8}, NIL, typeof(usual))
        TestMacro(mc, e"{|a| a := U(1234+1), a }", <OBJECT>{8}, 1234+1, typeof(usual))
        TestMacro(mc, e"{|a| UU := U(1234+1), UU }", <OBJECT>{8}, 1234+1, typeof(usual))
        TestMacro(mc, e"{|a| a := \"abcdef\", a:ToUpperInvariant() }", <OBJECT>{8}, "ABCDEF", typeof(usual))
        TestMacro(mc, e"{|a| a := NIL }", <OBJECT>{8}, NIL, typeof(usual))
        TestMacro(mc, e"{|| I3(4,4,4) }", <OBJECT>{}, 12, typeof(usual))
        TestMacro(mc, e"{|a| a := I3(4,4,) }", <OBJECT>{}, 11, typeof(usual))
        TestMacro(mc, e"{|a| a := I3(4,,4) }", <OBJECT>{}, 10, typeof(usual))
        TestMacro(mc, e"{|a| a := I3(,4,4) }", <OBJECT>{}, 9, typeof(usual))
        TestMacro(mc, e"{|a| a := I3(,,) }", <OBJECT>{}, 6, typeof(usual))
        TestMacro(mc, e"{|a| a := I3(4,4) }", <OBJECT>{}, 11, typeof(usual))
        TestMacro(mc, e"{|a| a := I3(4) }", <OBJECT>{}, 9, typeof(usual))
        TestMacro(mc, e"{|a| a := I3() }", <OBJECT>{}, 6, typeof(usual))
        TestMacro(mc, e"{|a| a := I0() }", <OBJECT>{}, 123, typeof(int))
        TestMacro(mc, e"{|a| a := CC(1,2,3) }", <OBJECT>{}, 6, typeof(usual))
        TestMacro(mc, e"{|a| a := CC(1,2,3,4) }", <OBJECT>{}, 6, typeof(usual))
        TestMacro(mc, e"{|a| a := CC() }", <OBJECT>{}, 0, typeof(usual))
        TestMacro(mc, e"{|a| a := CC(1,2) }", <OBJECT>{}, 3, typeof(usual))
        TestMacro(mc, e"{|a| a := CC(,1,2) }", <OBJECT>{}, 3, typeof(usual))
        TestMacro(mc, e"{|a| a := CC(1,,2) }", <OBJECT>{}, 3, typeof(usual))
        TestMacro(mc, e"{|a| a := U({1,2,3}) }", <OBJECT>{}, {1,2,3}, typeof(usual))
        TestMacro(mc, e"{|a| a := U({1,,2,,}) }", <OBJECT>{}, {1,NIL,2,NIL,NIL}, typeof(usual))
        TestMacro(mc, e"{|a| a := <INT>{1,2,3} }", <OBJECT>{}, <INT>{1,2,3}, typeof(int[]))
//        TestMacro(mc, e"{|a| a := A(123) }", <OBJECT>{}, 3, typeof(usual)) // FAIL - A accepts byref arg
//        TestMacro(mc, "{|a|a := 8, a := 8**a}", <OBJECT>{123}, 2<<24, typeof(real)) // FAIL
//        TestMacro(mc, e"{|a| a:ToString() }", <OBJECT>{8}, "8", typeof(string)) // FAIL - String:ToString() is overloaded!
//        TestMacro(mc, "I((int)123.456)", <OBJECT>{}, 123, typeof(int)) //FAIL

        Console.WriteLine("Total pass: {0}/{1}", TotalSuccess, TotalTests)
        return

    function RunPerf(mc as XSharp.Runtime.MacroCompiler, src as string) as void
        TestMacroCompiler(mc, src, 15, true, false)
        TestMacroCompiler(mc, src, 15, true, true)
        TestMacroCompiler(mc, src, 100000, false, false)
        TestMacroCompiler(mc, src, 100000, false, true)
        return

    function TestMacro(mc as XSharp.Runtime.MacroCompiler, src as string, args as object[], expect as usual, t as Type) as logic
        TotalTests += 1
        Console.Write("Test: '{0}' ", src)
        var cb := mc:Compile(src)
        var res := cb:EvalBlock(args)
        var match := res = expect
        if IsArray(expect)
            match := ALen(expect) = ALen((usual)res)
            for var i := 1 to ALen(expect)
                if expect[i] != ((usual)res)[i]
                    match := false
                end
            next
        elseif t != null .and. t:IsArray
            local e := expect as object
            match := e:Length = res:Length .and. t == res?:GetType()
            local m := t:GetMethod("GetValue",<Type>{typeof(int)}) as System.Reflection.MethodInfo
            for var i := 1 to e:Length
                var ve := m:Invoke(e,<object>{i-1})
                var vr := m:Invoke(res,<object>{i-1})
                if !Object.Equals(ve,vr)
                    match := false
                end
            next
        end
        if (match) .and. ((t == null) || (t == res?:GetType()))
            TotalSuccess += 1
            Console.WriteLine("[OK]")
            return true
        else
            TotalFails += 1
            Console.WriteLine("[FAIL] (res = {0}, type = {1})", res, res?:GetType())
        end
        return false

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
