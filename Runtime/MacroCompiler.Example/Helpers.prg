// Helpers.prg
// Created by    : nvk
// Creation Date : 2/13/2021 4:00:44 PM
// Created for   :
// WorkStation   : I7


USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharp.Runtime
USING XSharp.MacroCompiler

BEGIN NAMESPACE MacroCompilerTest

    FUNCTION ParseScript(mc AS XSharp.Runtime.MacroCompiler, src AS STRING) AS VOID
        Console.WriteLine("Parsing script ...")
        VAR ast := mc:GetObjectCompiler(true):Parse(src)
        Console.WriteLine(ast)

    FUNCTION ParseMacro(mc AS XSharp.Runtime.MacroCompiler, src AS STRING) AS VOID
        Console.WriteLine("Parsing macro ...")
        VAR ast := mc:GetObjectCompiler(true):Parse(src)
        Console.WriteLine(ast)

    FUNCTION EvalMacro(mc AS XSharp.Runtime.MacroCompiler, src AS STRING, args PARAMS OBJECT[]) AS USUAL
        Console.WriteLine("Executing macro ...")
        TRY
            //var cb := MCompile(src)
            VAR cb := mc:Compile(src)
            VAR res := cb:EvalBlock(args)
            Console.WriteLine("res = {0}",res)
            RETURN res
        CATCH e AS CompilationError
            Console.WriteLine("{0}",e:Message)
            RETURN NIL
        END

    GLOBAL TotalFails := 0 AS INT
    GLOBAL TotalTests := 0 AS INT
    GLOBAL TotalSuccess := 0 AS INT

    FUNCTION RunPerf(mc AS XSharp.Runtime.MacroCompiler, src AS STRING) AS VOID
        TestMacroCompiler(mc, src, 15, TRUE, FALSE)
        TestMacroCompiler(mc, src, 15, TRUE, TRUE)
        TestMacroCompiler(mc, src, 100000, FALSE, FALSE)
        TestMacroCompiler(mc, src, 10000, FALSE, TRUE)
        RETURN

    FUNCTION TestParse(mc AS XSharp.Runtime.MacroCompiler, src AS STRING, val AS STRING) AS LOGIC
        TotalTests += 1
        Console.Write("Test: '{0}' ", src)
        VAR res := mc:GetObjectCompiler(true):Parse(src):ToString()
        IF res = val
            TotalSuccess += 1
            Console.WriteLine("[OK]")
            RETURN TRUE
        ELSE
            TotalFails += 1
            Console.WriteLine("[FAIL] ({0} != {1})", res, val)
            wait
        END
        RETURN FALSE

    FUNCTION TestMacroU(mc AS XSharp.Runtime.MacroCompiler, src AS STRING, args AS USUAL[], expect AS USUAL, t AS Type, ec := ErrorCode.NoError AS ErrorCode) AS LOGIC
        TRY
            TotalTests += 1
            Console.Write("Test: '{0}' ", src)

            //local isCb, memVars as logic
            //VAR rtc := mc:Compile(src, true, null, ref isCb, ref memVars)
            //VAR cb := XSharp._Codeblock{rtc, src, isCb, memVars}
            VAR cb := mc:CompileCodeblock(src)

            VAR res := cb:Eval(args)
            LOCAL match AS LOGIC
            IF IsArray(expect)
                match := ALen(expect) = ALen(res)
                FOR VAR i := 1 TO ALen(expect)
                    IF expect[i] != ((ARRAY)res)[i]
                        match := FALSE
                    END
                NEXT
            ELSEIF t != NULL .AND. t:IsArray
                LOCAL e := expect AS OBJECT
                match := e:Length = res:Length .AND. t == res:GetType()
                LOCAL m := t:GetMethod("GetValue",<Type>{typeof(INT)}) AS System.Reflection.MethodInfo
                FOR VAR i := 1 TO e:Length
                    VAR ve := m:Invoke(e,<OBJECT>{i-1})
                    VAR vr := m:Invoke(res,<OBJECT>{i-1})
                    IF !Object.Equals(ve,vr)
                        match := FALSE
                    END
                NEXT
            ELSEIF t == typeof(OBJECT)
                match := TRUE
            ELSE
                TRY
                    LOCAL r := res AS DYNAMIC
                    LOCAL e := expect AS DYNAMIC
                    match := r = e .OR. res = expect
                CATCH
                    match := res = expect
                END
            END
            IF (ec == ErrorCode.NoError) .AND. (match) .AND. ((t == NULL) || (t == res:GetType()) || (t == typeof(OBJECT) .AND. res == NULL .AND. expect == NULL))
                TotalSuccess += 1
                Console.WriteLine("[OK]")
                RETURN TRUE
            ELSE
                TotalFails += 1
                Console.WriteLine("[FAIL] (res = {0}, type = {1}, no error)", res, res:GetType())
                wait
            END
            RETURN FALSE
        CATCH e AS CompilationError
            IF e:@@Code == ec
                TotalSuccess += 1
                Console.WriteLine("[OK]")
                RETURN TRUE
            ELSE
                TotalFails += 1
                Console.WriteLine("[FAIL] ({0})", e:Message)
                wait
            END
            RETURN FALSE
        CATCH e AS Exception
            IF t == e:GetType() .AND. e:Message == expect
                TotalSuccess += 1
                Console.WriteLine("[OK]")
                RETURN TRUE
            ELSE
                TotalFails += 1
                Console.WriteLine("[FAIL] (Exception: {0})", e:Message)
                wait
                RETURN FALSE
            END
        END

    FUNCTION TestMacro(mc AS XSharp.Runtime.MacroCompiler, src AS STRING, args AS OBJECT[], expect AS USUAL, t AS Type, ec := ErrorCode.NoError AS ErrorCode) AS LOGIC
        TRY
            TotalTests += 1
            Console.Write("Test: '{0}' ", src)

            //local isCb, memVars as logic
            //VAR rtc := mc:Compile(src, true, null, ref isCb, ref memVars)
            //VAR cb := XSharp._Codeblock{rtc, src, isCb, memVars}
            VAR cb := mc:Compile(src)

            VAR res := cb:EvalBlock(args)
            LOCAL match AS LOGIC
            IF IsArray(expect)
                match := ALen(expect) = ALen(res)
                FOR VAR i := 1 TO ALen(expect)
                    IF expect[i] != ((ARRAY)res)[i]
                        match := FALSE
                    END
                NEXT
            ELSEIF t != NULL .AND. t:IsArray
                LOCAL e := expect AS OBJECT
                match := e:Length = res:Length .AND. t == res?:GetType()
                LOCAL m := t:GetMethod("GetValue",<Type>{typeof(INT)}) AS System.Reflection.MethodInfo
                FOR VAR i := 1 TO e:Length
                    VAR ve := m:Invoke(e,<OBJECT>{i-1})
                    VAR vr := m:Invoke(res,<OBJECT>{i-1})
                    IF !Object.Equals(ve,vr)
                        match := FALSE
                    END
                NEXT
            ELSEIF t == typeof(OBJECT)
                match := TRUE
            ELSE
                TRY
                    LOCAL r := res AS DYNAMIC
                    LOCAL e := expect AS DYNAMIC
                    match := r = e .OR. res = expect
                CATCH
                    match := res = expect
                END
            END
            IF (ec == ErrorCode.NoError) .AND. (match) .AND. ((t == NULL) || (t == res?:GetType()) || (t == typeof(OBJECT) .AND. res == NULL .AND. expect == NULL))
                TotalSuccess += 1
                Console.WriteLine("[OK]")
                RETURN TRUE
            ELSE
                TotalFails += 1
                Console.WriteLine("[FAIL] (res = {0}, type = {1}, no error)", res, res?:GetType())
                wait
            END
            RETURN FALSE
        CATCH e AS CompilationError
            IF e:@@Code == ec
                TotalSuccess += 1
                Console.WriteLine("[OK]")
                RETURN TRUE
            ELSE
                TotalFails += 1
                Console.WriteLine("[FAIL] ({0})", e:Message)
                wait
            END
            RETURN FALSE
        catch e as Exception
            IF t == e:GetType() .AND. e:Message == expect
                TotalSuccess += 1
                Console.WriteLine("[OK]")
                RETURN TRUE
            ELSE
                TotalFails += 1
                Console.WriteLine("[FAIL] (Exception: {0})", e:Message)
                wait
                return false
            END
        END

    FUNCTION CreateFoxScriptCompiler() AS XSharp.Runtime.MacroCompiler
        var o := XSharp.MacroCompiler.MacroOptions.FoxPro
        o:ParseMode := ParseMode.Statements
        RETURN XSharp.Runtime.MacroCompiler{o}

    FUNCTION CreateFoxMacroCompiler() AS XSharp.Runtime.MacroCompiler
        RETURN XSharp.Runtime.MacroCompiler{XSharp.MacroCompiler.MacroOptions.FoxPro}

    FUNCTION CreateScriptCompiler() AS XSharp.Runtime.MacroCompiler
        var o := XSharp.MacroCompiler.MacroOptions.Default
        o:ParseMode := ParseMode.Statements
        RETURN XSharp.Runtime.MacroCompiler{o}

    FUNCTION CreateMacroCompiler() AS XSharp.Runtime.MacroCompiler
        Console.WriteLine("Creating macro compiler ...")

        VAR m := GC:GetTotalMemory(TRUE)
        VAR t := DateTime.Now

        VAR mc := XSharp.Runtime.MacroCompiler{XSharp.MacroCompiler.MacroOptions.Default}

        VAR dt := DateTime.Now - t
        t += dt

        Console.WriteLine("  Completed in {0}", dt)
        Console.WriteLine("  Memory: +{0} bytes", GC.GetTotalMemory(FALSE) - m)
        Console.WriteLine()

        mc:Compile("")
        RETURN mc

    FUNCTION ReportMemory(description AS STRING) AS VOID
        Console.WriteLine("Memory: {0} ({1})", GC:GetTotalMemory(TRUE),description)
        Console.WriteLine()
        RETURN

    FUNCTION TestMacroCompiler(mc AS XSharp.Runtime.MacroCompiler, source AS STRING, iterations AS INT, check_mem AS LOGIC, compile AS LOGIC) AS VOID
        Console.WriteLine("Start {0} {1} ({2} iterations) ...", IIF(compile,"compiler","parser"), IIF(check_mem,"memory test","benchmark"), iterations);

        VAR m := GC:GetTotalMemory(TRUE)
        VAR t := DateTime.Now

        FOR VAR i := 0 TO iterations
            LOCAL m0 := 0 AS INT64
            IF (check_mem)
                m0 := GC:GetTotalMemory(FALSE)
            END

            IF (compile)
                VAR o := MCompile(source)
                //var o := mc:Compile(source)
                //o:Eval()
            ELSE
                VAR ast := mc:GetObjectCompiler(true):Parse(source)
                //Console.WriteLine(ast);
            END

            IF (check_mem)
                Console.WriteLine("  Iteration {0} memory: +{1} bytes", i+1, GC:GetTotalMemory(FALSE) - m0)
            END
        NEXT

        VAR dt := DateTime.Now - t
        t += dt
        IF (!check_mem)
            Console.WriteLine("  Completed in {0} ({1} ms/iter, {2:#} iters/sec)", dt, dt:TotalMilliseconds/iterations, iterations/dt:TotalSeconds)
            Console.WriteLine("  Memory: +{0} bytes", GC:GetTotalMemory(FALSE) - m)
        END

        Console.WriteLine()
        RETURN

END NAMESPACE

