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

function A(i ref object) as int
    var v := i astype int? DEFAULT 0
    i := 1000 + v
    return v

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

class testclassdc
    v1 as int
    v2 as string

    operator ==(o1 as testclassdc, o2 as testclassdc) as logic
        return o1:v1 == o2:v1 .and. o1:v2 == o2:v2
end class

class testclass
    v1 as int
    v2 as string

    static property sprop as int auto get set
    property prop as int auto get set

    constructor()
    constructor(i as int)
        v1 := i
        prop := i
    operator ==(o1 as testclass, o2 as testclass) as logic
        return o1:v1 == o2:v1 .and. o1:v2 == o2:v2
end class

struct teststruct
    v1 as int
    v2 as string

    static property sprop as int auto get set
    property prop as int auto get set

    constructor(i as int)
        v1 := i
        prop := i
    operator ==(o1 as teststruct, o2 as teststruct) as logic
        return o1:v1 == o2:v1 .and. o1:v2 == o2:v2
end struct

global tsi := teststruct{1} as teststruct

global tci := testclass{1} as testclass

begin namespace MacroCompilerTest
    using XSharp.Runtime
    using XSharp.MacroCompiler

	function Start() as void
	    SetMacroCompiler(typeof(XSharp.Runtime.MacroCompiler))

        ReportMemory("initial")
        var mc := CreateMacroCompiler()

        EvalMacro(mc, e"{|a| a := testclass{}, a:v1 := 1 }", 8)
        wait

        RunTests(mc)
        wait

        RunPerf(mc, "Console.WriteLine(123)")

        ReportMemory("final");

        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()

    function EvalMacro(mc as XSharp.Runtime.MacroCompiler, src as string, args params object[]) as usual
        Console.WriteLine("Executing macro ...")
        //var cb := MCompile(src)
        var cb := mc:Compile(src)
        var res := cb:EvalBlock(args)
        Console.WriteLine("res = {0}",res)
        return res

    global TotalFails := 0 as int
    global TotalTests := 0 as int
    global TotalSuccess := 0 as int

    function RunTests(mc as XSharp.Runtime.MacroCompiler) as void
        Console.WriteLine("Running tests ...")

        TestMacro(mc, "#HELLo", <OBJECT>{}, #hello, typeof(symbol))
        TestMacro(mc, "#HELLo + #World", <OBJECT>{}, #hello + #world, typeof(string))
        TestMacro(mc, e"#HELLo + \"world\"", <OBJECT>{}, #hello + "world", typeof(string))
        TestMacro(mc, e"\"Hello\" + #world", <OBJECT>{}, "Hello" + #world, typeof(string))
        TestMacro(mc, "U(12345)", <OBJECT>{}, 12345, typeof(usual))
        TestMacro(mc, "U(U(12345)-1)", <OBJECT>{}, 12344, typeof(usual))
        TestMacro(mc, "I(123+45)", <OBJECT>{}, 123+45, typeof(int))
        TestMacro(mc, "R(123)", <OBJECT>{}, 123, typeof(real8))
        TestMacro(mc, "R(123.456)", <OBJECT>{}, 123.456, typeof(real8))
        TestMacro(mc, "U(123.456)", <OBJECT>{}, 123.456, typeof(usual))
        TestMacro(mc, "123.456", <OBJECT>{}, 123.456, typeof(float))
        TestMacro(mc, "123.456s", <OBJECT>{}, 123.456s, typeof(real4))
        TestMacro(mc, "123.456d", <OBJECT>{}, 123.456d, typeof(real8))
        TestMacro(mc, "123.450m", <OBJECT>{}, 123.450m, typeof(decimal))
        TestMacro(mc, "123.450m+321", <OBJECT>{}, 444.450m, typeof(decimal))
        TestMacro(mc, "123.450m+321.05", <OBJECT>{}, 444.500m, typeof(decimal))
        TestMacro(mc, "{|a,b,c|a := b := 1343+1}", <OBJECT>{}, 1343+1, typeof(int))
        TestMacro(mc, "{|a,b,c|}", <OBJECT>{}, null, null)
        TestMacro(mc, "{|a,b,c|1234}", <OBJECT>{}, 1234, typeof(int))
        TestMacro(mc, "1234", <OBJECT>{}, 1234, typeof(int))
        TestMacro(mc, "12 == 12", <OBJECT>{}, true, typeof(logic))
        TestMacro(mc, "", <OBJECT>{}, null, null)
        TestMacro(mc, "2018.12.31", <OBJECT>{}, 2018.12.31, typeof(date))
        TestMacro(mc, "2018.1.1", <OBJECT>{}, 2018.1.1, typeof(date))
        TestMacro(mc, "2018.12.31 == 2018.12.31", <OBJECT>{}, true, typeof(logic))
        TestMacro(mc, "2018.12.31 = 2018.12.31", <OBJECT>{}, true, typeof(logic))
        TestMacro(mc, "2018.12.31 = 2018.1.1", <OBJECT>{}, false, typeof(logic))
        TestMacro(mc, "2018.12.31 != 2018.12.31", <OBJECT>{}, false, typeof(logic))
        TestMacro(mc, "2018.12.31 != 2018.1.1", <OBJECT>{}, true, typeof(logic))
        TestMacro(mc, "null", <OBJECT>{}, null, null)
        TestMacro(mc, "null_object", <OBJECT>{}, null_object, null)
        TestMacro(mc, "null_string", <OBJECT>{}, null_string, null)
        TestMacro(mc, "null_psz = psz._NULL_PSZ", <OBJECT>{}, true, typeof(logic))
        TestMacro(mc, "null_symbol", <OBJECT>{}, null_symbol, typeof(symbol))
        TestMacro(mc, "null_date", <OBJECT>{}, null_date, typeof(date))
        TestMacro(mc, "null_codeblock", <OBJECT>{}, null_codeblock, null)
        TestMacro(mc, "null_ptr", <OBJECT>{}, null_ptr, null)
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
        TestMacro(mc, e"{|a| (0 > 1) .and. (0 < 1) }", <OBJECT>{8}, false, typeof(logic))
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
        TestMacro(mc, e"object{}", <OBJECT>{}, object{}, typeof(object))
        TestMacro(mc, e"teststruct{12}", <OBJECT>{}, teststruct{12}, typeof(teststruct))
        TestMacro(mc, e"teststruct{}", <OBJECT>{}, teststruct{}, typeof(teststruct))
        TestMacro(mc, e"testclass{}", <OBJECT>{}, testclass{}, typeof(testclass))
        TestMacro(mc, e"testclass{23}", <OBJECT>{}, testclass{23}, typeof(testclass))
        TestMacro(mc, e"testclassdc{}", <OBJECT>{}, testclassdc{}, typeof(testclassdc))
        TestMacro(mc, e"int{}", <OBJECT>{}, 0, typeof(int))
        TestMacro(mc, e"{|z| A(z) }", <OBJECT>{123}, 123, typeof(int32))
        TestMacro(mc, e"{|z| A(z), z }", <OBJECT>{123}, 1123, typeof(int32))
        TestMacro(mc, e"{|a| testclass.sprop := 555, a := ++testclass.sprop }", <OBJECT>{}, 556, typeof(int32))
        TestMacro(mc, e"{|a| testclass.sprop := a, a := ++testclass.sprop }", <OBJECT>{55}, 56, typeof(int32))
        TestMacro(mc, e"{|a| teststruct.sprop := 555, a := ++teststruct.sprop }", <OBJECT>{}, 556, typeof(int32))
        TestMacro(mc, e"{|a| teststruct.sprop := a, a := ++teststruct.sprop }", <OBJECT>{55}, 56, typeof(int32))
        TestMacro(mc, e"{|a| a := testclass{}, a:prop }", <OBJECT>{}, 0, typeof(usual))
        TestMacro(mc, e"{|a| a := testclass{222}, a:prop }", <OBJECT>{}, 222, typeof(usual))
        TestMacro(mc, e"{|a| a := teststruct{}, a:prop }", <OBJECT>{}, 0, typeof(usual))
        TestMacro(mc, e"{|a| a := teststruct{222}, a:prop }", <OBJECT>{}, 222, typeof(usual))
        TestMacro(mc, e"{|a| a := testclass{}, a:v1 := 1 }", <OBJECT>{}, 1, typeof(usual))
        TestMacro(mc, e"{|a| a := testclass{222}, a:v1 }", <OBJECT>{}, 222, typeof(usual))
        TestMacro(mc, e"{|a| a := teststruct{}, a:v1 := 1 }", <OBJECT>{}, 1, typeof(usual))
        TestMacro(mc, e"{|a| a := teststruct{222}, a:v1 }", <OBJECT>{}, 222, typeof(usual))
        TestMacro(mc, e"{|a| a := testclass{}, a:prop := 111 }", <OBJECT>{}, 111, typeof(usual))
        TestMacro(mc, e"{|a,b| b := testclass{}, b:prop := a, ++b:prop }", <OBJECT>{55}, 56, typeof(usual))
        TestMacro(mc, e"{|| tsi:v1 }", <OBJECT>{}, 1, typeof(usual))
        TestMacro(mc, e"{|| ++tsi:v1 }", <OBJECT>{}, 2, typeof(usual))
//        TestMacro(mc, e"{|| tsi:v1 := 10, tsi:v1 }", <OBJECT>{}, 10, typeof(usual)) // FAIL because tsi is boxed by value
        TestMacro(mc, e"{|| ++tsi:prop }", <OBJECT>{}, 2, typeof(usual))
        TestMacro(mc, e"{|| tci:v1 }", <OBJECT>{}, 1, typeof(usual))
        TestMacro(mc, e"{|| ++tci:v1 }", <OBJECT>{}, 2, typeof(usual))
        TestMacro(mc, e"{|| ++tci:prop }", <OBJECT>{}, 2, typeof(usual))
        TestMacro(mc, e"{|| tci:v1 := 10, tci:v1++, tci:v1 }", <OBJECT>{}, 11, typeof(usual))
        TestMacro(mc, e"{|a| IIF(a>10,123,1.23) }", <OBJECT>{100}, 123, typeof(float))
        TestMacro(mc, e"{|a| IIF(a>10,123,1.23) }", <OBJECT>{1}, 1.23, typeof(float))
        TestMacro(mc, e"{|a| IIF(a>10,1) }", <OBJECT>{100}, 1, typeof(usual))
        TestMacro(mc, e"{|a| IIF(a>10,,1) }", <OBJECT>{100}, nil, typeof(usual))
        TestMacro(mc, e"{|a| (float)++a/2 }", <OBJECT>{2}, 1.5, typeof(float))
        TestMacro(mc, e"{|a| a := {1,2,3,4}, a[1] := 10, a[1] + a[2] }", <OBJECT>{}, 12, typeof(usual))
        TestMacro(mc, e"{|a| a := {1,2,3,4}, a[1] += 10, a[1] }", <OBJECT>{}, 11, typeof(usual))
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
        local match as logic
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
        elseif t == typeof(object)
            match := true
        else
            try
                local r := res as dynamic
                local e := expect as dynamic
                match := r = e .or. res = expect
            catch
                match := res = expect
            end
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
