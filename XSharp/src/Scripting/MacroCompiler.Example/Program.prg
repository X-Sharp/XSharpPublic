using System
using System.Collections.Generic
using System.Linq
using System.Text

begin namespace MacroCompilerTest
    using XSharp.Runtime
    using XSharp.MacroCompiler

	function Start() as void
        var test_source := "Console.WriteLine(123)";

        ReportMemory("initial")
        var mc := CreateMacroCompiler()
        TestMacroCompiler(mc, test_source, 15, true, false)
        TestMacroCompiler(mc, test_source, 15, true, true)
        TestMacroCompiler(mc, test_source, 100000, false, false)
        TestMacroCompiler(mc, test_source, 100000, false, true)
        ReportMemory("final");

        Console.WriteLine("Press any key to continue...")
        Console.ReadKey()

    function CreateMacroCompiler() as MacroCompiler
        Console.WriteLine("Creating macro compiler ...")

        var m := GC:GetTotalMemory(true)
        var t := DateTime.Now

        var mc := MacroCompiler{}

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

    function TestMacroCompiler(mc as MacroCompiler, source as string, iterations as int, check_mem as logic, compile as logic) as void
        Console.WriteLine("Start {0} {1} ({2} iterations) ...", iif(compile,"compiler","parser"), iif(check_mem,"memory test","benchmark"), iterations);

        var m := GC:GetTotalMemory(true)
        var t := DateTime.Now

        for var i := 0 to iterations
            local m0 := 0 as int64
            if (check_mem)
                m0 := GC:GetTotalMemory(FALSE)
            end

            if (compile)
                var o := mc:Compile(source)
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
            Console.WriteLine("  Completed in {0} ({1} ms/iter)", dt,dt:TotalMilliseconds/iterations)
            Console.WriteLine("  Memory: +{0} bytes", GC:GetTotalMemory(false) - m)
        end

        Console.WriteLine()
        return

end namespace


begin namespace XSharp.Runtime
    using XSharp.MacroCompiler

    delegate MacroDelegate(args params dynamic[]) as dynamic

    public class RuntimeCodeblock implements Vulcan.Runtime.ICodeBlock
        private _eval as MacroDelegate

        public method Eval(args params dynamic[]) as dynamic
            return _eval(args)

        public constructor(evalMethod as MacroDelegate)
            _eval := evalMethod
    end class

    public class MacroCompiler implements Vulcan.Runtime.IMacroCompiler
        internal compiler := Compilation.Create<dynamic,MacroDelegate>() as Compilation<dynamic,MacroDelegate>

	    public method Compile (cMacro as string, lOldStyle as logic, Module as System.Reflection.Module, lIsBlock ref logic) as Vulcan.Runtime.ICodeBlock
		    lIsBlock := cMacro:StartsWith("{|")
    	    return RuntimeCodeblock{compiler.Compile(cMacro)}

	    public method Compile (cMacro as string) as Vulcan.Runtime.ICodeBlock
    	    return RuntimeCodeblock{compiler.Compile(cMacro)}
    end class

end namespace


begin namespace Vulcan.Runtime
    interface ICodeBlock
        public method Eval(args params dynamic[]) as dynamic
    end interface

    interface IMacroCompiler
	    method Compile (cMacro as string, lOldStyle as logic, Module as System.Reflection.Module, lIsBlock ref logic) as ICodeBlock
    end interface
end namespace
