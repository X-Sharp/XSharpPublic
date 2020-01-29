USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharp.Runtime
USING XSharp.MacroCompiler

FUNCTION Args(args PARAMS OBJECT[]) AS OBJECT[]
    RETURN args

FUNCTION U(u AS USUAL) AS USUAL
    RETURN u

FUNCTION U(s AS STRING) AS USUAL
    RETURN s

FUNCTION R(r AS REAL8) AS REAL8
    RETURN r

FUNCTION I(i AS INT) AS INT
    RETURN i

FUNCTION A(i REF OBJECT) AS INT
    VAR v := i ASTYPE INT? DEFAULT 0
    i := 1000 + v
    RETURN v

FUNCTION AllTrim(cValue AS STRING) AS STRING
    RETURN "MyAlltrim()"

FUNCTION I0() AS INT
    RETURN 123;

FUNCTION I3(a := 1 AS INT, b := 2 AS INT, c := 3 AS INT)
    RETURN a+b+c

FUNCTION CC(a,b,c)
    IF a == NIL
        a := 0
    END
    IF b == NIL
        b := 0
    END
    IF c == NIL
        c := 0
    END
    RETURN a+b+c

GLOBAL UU AS USUAL

CLASS testclassdc
    PUBLIC v1 AS INT
    PUBLIC v2 AS STRING

    OVERRIDE METHOD GetHashCode() AS INT
        RETURN SUPER:GetHashCode()
    OVERRIDE METHOD Equals(o AS OBJECT) AS LOGIC
        RETURN SELF == o
    OPERATOR ==(o1 AS testclassdc, o2 AS testclassdc) AS LOGIC
        RETURN o1:v1 == o2:v1 .AND. o1:v2 == o2:v2
    OPERATOR !=(o1 AS testclassdc, o2 AS testclassdc) AS LOGIC
        RETURN !(o1 == o2)
END CLASS

CLASS testbase
    METHOD BString() AS STRING
        RETURN "bbase"

    METHOD nString(x AS LONG) AS STRING
        RETURN "base"

    VIRTUAL METHOD FString() AS STRING
        RETURN "base"
END CLASS

CLASS testclass INHERIT testbase
    CLASS nested
        ENUM child
            haha := 4321
            blabla := 1
        END ENUM
        ENUM child2
            haha := 432
            blabla := 12
        END ENUM

        PUBLIC STATIC ttt := child.blabla AS child

        PUBLIC STATIC fff := 333 AS INT

        PUBLIC CONST ccc := 456 AS INT

        PUBLIC CONST eee := child.blabla AS child
    END CLASS

    PUBLIC v1 AS INT
    PUBLIC v2 AS STRING

    STATIC PROPERTY sprop AS INT AUTO GET SET
    PROPERTY prop AS INT AUTO GET SET

    CONSTRUCTOR()
    CONSTRUCTOR(i AS INT)
        v1 := i
        prop := i

    METHOD UString() AS STRING
        RETURN v1:ToString()

    METHOD NString(x AS LONG) AS STRING
        RETURN "child"

    OVERRIDE METHOD FString() AS STRING
        RETURN v1:ToString()
    METHOD FString(prefix AS STRING) AS STRING
        RETURN prefix+v1:ToString()

    OVERRIDE METHOD GetHashCode() AS INT
        RETURN SUPER:GetHashCode()
    OVERRIDE METHOD Equals(o AS OBJECT) AS LOGIC
        RETURN SELF == o
    OPERATOR ==(o1 AS testclass, o2 AS testclass) AS LOGIC
        RETURN o1:v1 == o2:v1 .AND. o1:v2 == o2:v2
    OPERATOR !=(o1 AS testclass, o2 AS testclass) AS LOGIC
        RETURN !(o1 == o2)
END CLASS

STRUCT teststruct
    PUBLIC v1 AS INT
    PUBLIC v2 AS STRING

    STATIC PROPERTY sprop AS INT AUTO GET SET
    PROPERTY prop AS INT AUTO GET SET

    CONSTRUCTOR(i AS INT)
        v1 := i
        v2 := NULL
        prop := i

    METHOD UString() AS STRING
        RETURN v1:ToString()

    METHOD FString() AS STRING
        RETURN v1:ToString()
    METHOD FString(prefix AS STRING) AS STRING
        RETURN prefix+v1:ToString()

    OVERRIDE METHOD GetHashCode() AS INT
        RETURN SUPER:GetHashCode()
    OVERRIDE METHOD Equals(o AS OBJECT) AS LOGIC
        RETURN SELF == (teststruct)o
    OPERATOR ==(o1 AS teststruct, o2 AS teststruct) AS LOGIC
        RETURN o1:v1 == o2:v1 .AND. o1:v2 == o2:v2
    OPERATOR !=(o1 AS teststruct, o2 AS teststruct) AS LOGIC
        RETURN !(o1 == o2)
END STRUCT

CLASS TestWithItem
    METHOD Item()
        RETURN 42
    PROPERTY Nested AS USUAL GET TestWithItem{}
END CLASS

CLASS TestWithItem2
    PROPERTY Item AS LONG  GET 42
    PROPERTY Nested AS USUAL GET TestWithItem2{}
END CLASS

GLOBAL tsi := teststruct{1} AS teststruct

GLOBAL tci := testclass{1} AS testclass

GLOBAL wag := "" AS STRING

FUNCTION MyVarGet(name AS STRING) AS USUAL
    RETURN wag + "VarGet(" + name + ")"

FUNCTION MyVarPut(name AS STRING, VALUE AS USUAL) AS USUAL
    RETURN wag + "VarPut(" + name +"):" + VALUE:ToString()

FUNCTION MyMemVarGet(name AS STRING) AS USUAL
    RETURN "MemVarGet(" + name + ")"

FUNCTION MyMemVarPut(name AS STRING, VALUE AS USUAL) AS USUAL
    RETURN "MemVarPut(" + name +"):" + VALUE:ToString()

FUNCTION MyFieldGet(name AS STRING) AS USUAL
    RETURN wag + "FieldGet(" + name + ")"

FUNCTION MyFieldSet(name AS STRING, VALUE AS USUAL) AS USUAL
    RETURN wag + "FieldSet(" + name +"):" + VALUE:ToString()

FUNCTION MyFieldGetWa(wa AS STRING, name AS STRING) AS USUAL
    RETURN "FieldGet(" + wa + "," + name + ")"

FUNCTION MyFieldSetWa(wa AS STRING, name AS STRING, VALUE AS USUAL) AS USUAL
    RETURN "FieldSet(" + wa + "," + name +"):" + VALUE:ToString()

FUNCTION MyPushWa(wa AS USUAL) AS VOID
    wag := wa + "->"
    RETURN

FUNCTION MyPopWa() AS VOID
    wag := ""
    RETURN

FUNCTION MyDbDo(op AS STRING) AS USUAL
    RETURN wag + "Do(" + op +")"

FUNCTION DoTest(n AS INT, l AS LOGIC, o AS System.Collections.ArrayList) AS INT
    RETURN n * 5

FUNCTION DoTestC(n AS INT, l AS LOGIC, o AS testclass) AS INT
    RETURN o:v1

FUNCTION DoTestS(n AS INT, l AS LOGIC, o AS teststruct) AS INT
    RETURN o:v1

CLASS ctest
    PUBLIC PROPERTY fld AS ctest AUTO
    PUBLIC a AS REAL8
    PUBLIC b AS REAL8
    CONSTRUCTOR (a_ AS REAL8, b_ AS REAL8)
        a := a_
        b := b_
    CONSTRUCTOR (o AS ctest)
        fld := o
    METHOD fieldget(s AS STRING)
        RETURN s
    METHOD propget(s AS STRING, m AS STRING)
        RETURN s + m
    OPERATOR ==(o1 AS ctest, o2 AS ctest) AS LOGIC
        RETURN o1:a == o2:a .AND. o1:b == o2:b
    OPERATOR !=(o1 AS ctest, o2 AS ctest) AS LOGIC
        RETURN !(o1 == o2)
END CLASS

CLASS Foo
   CONSTRUCTOR() CLIPPER
    OPERATOR ==(o1 AS Foo, o2 AS Foo) AS LOGIC
        RETURN TRUE
    OPERATOR !=(o1 AS Foo, o2 AS Foo) AS LOGIC
        RETURN !(o1 == o2)
END CLASS

FUNCTION testDef(cFormularName AS STRING, uDataSource := NIL AS USUAL, ;
uVorlage := "" AS USUAL, cOeffnenModus := "F9" AS USUAL, uParameter := NIL AS USUAL, ;
oDataContext := NULL AS OBJECT, cWinMode := NULL AS USUAL, oProtype := NULL AS OBJECT, ;
oDBVorlage := NULL AS OBJECT ) AS USUAL STRICT
    RETURN "testDef"

FUNC TestInt32(n AS INT) AS INT
RETURN n
FUNC TestDWord(n AS DWORD) AS DWORD
RETURN n

FUNC testRet(x AS STRING) AS STRING
RETURN x

GLOBAL Error := 321 AS INT

GLOBAL ErrorLevel := 1 AS INT

CLASS ErrString
    STATIC V := 333 AS INT
END CLASS

BEGIN NAMESPACE TestNS.Nested
    CLASS TestClass
    END CLASS
END NAMESPACE

CLASS TestNS2.TestClass
	CONSTRUCTOR()
	STATIC METHOD TestMethod() AS INT STRICT
	RETURN 123
END CLASS

BEGIN NAMESPACE MacroCompilerTest

	FUNCTION Start() AS VOID
	    SetMacroCompiler(typeof(XSharp.Runtime.MacroCompiler))

        ReportMemory("initial")
        VAR mc := CreateMacroCompiler()

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

        RunTests(mc)
        WAIT

        RunPerf(mc, "Console.WriteLine(123)")

        ReportMemory("final");

        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()

    FUNCTION ParseMacro(mc AS XSharp.Runtime.MacroCompiler, src AS STRING) AS VOID
        Console.WriteLine("Parsing macro ...")
        VAR ast := mc:compiler:Parse(src)
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

    FUNCTION RunTests(mc AS XSharp.Runtime.MacroCompiler) AS VOID
        Console.WriteLine("Running tests ...")

        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsField
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGet, "MyFieldGet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSet, "MyFieldSet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGetWa, "MyFieldGetWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSetWa, "MyFieldSetWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___pushWorkarea, "MyPushWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___popWorkarea, "MyPopWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___MemVarGet, "MyMemVarGet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___MemVarPut, "MyMemVarPut")

        // USUAL defaults to FALSE for FoxPro
        TestMacro(mc, e"{|a| a := default(usual) }", Args(8), FALSE, typeof(LOGIC))

        // FoxPro dot access
        TestMacro(mc, e"{|| testclass{}.NString((byte)1) }", Args(), "child", typeof(STRING))
        TestMacro(mc, e"{|a| a := testclass{}, a.prop }", Args(), 0, typeof(INT))
        TestMacro(mc, e"{|| tsi.v1 }", Args(), 1, typeof(INT))
        TestMacro(mc, e"{|| tci.v1 := 10, tci.v1++, tci.v1 }", Args(), 11, typeof(INT))
        TestMacro(mc, e"{|| DEVS.NIKOS}", Args(), "FieldGet(DEVS,NIKOS)", typeof(STRING))
        TestMacro(mc, e"{|| DEVS.NIKOS := \"123\"}", Args(), "FieldSet(DEVS,NIKOS):123", typeof(STRING))
        TestMacro(mc, e"{|| M.NAME}", Args(), "MemVarGet(NAME)", typeof(STRING))
        TestMacro(mc, e"{|| M.NAME := \"Nikos\"}", Args(), "MemVarPut(NAME):Nikos", typeof(STRING))
        TestMacro(mc, e"{|| @@M.NAME}", Args(), "FieldGet(M,NAME)", typeof(STRING))
        TestMacro(mc, e"{|| @@M.NAME := \"Nikos\"}", Args(), "FieldSet(M,NAME):Nikos", typeof(STRING))
        TestMacro(mc, e"{|| Alltrim('abc')}", Args(), "MyAlltrim()", typeof(STRING))

        // FoxPro literal dates and datetimes. For simplicity we return them all as DateTime
        TestMacro(mc, e"{|| {^ 2019-12-31}", Args(), DateTime{2019,12,31}, typeof(DateTime))
        TestMacro(mc, e"{|| {^ 2019-12-31 11:12:13}", Args(), DateTime{2019,12,31,11,12,13}, typeof(DateTime))
        TestMacro(mc, e"{|| {^ 2019-12-31 11:12:13AM}", Args(), DateTime{2019,12,31,11,12,13}, typeof(DateTime))
        TestMacro(mc, e"{|| {^ 2019-12-31 11:12:13PM}", Args(), DateTime{2019,12,31,23,12,13}, typeof(DateTime))
        TestMacro(mc, e"{|| {^ 2019-12-31 11:12:13 PM}", Args(), DateTime{2019,12,31,23,12,13}, typeof(DateTime))
        // ForPro Logical operators, only works when main app is in FoxPro mode.
        TestMacro(mc, e"{|a,b| a AND b }", Args(TRUE, TRUE), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a AND b }", Args(FALSE, TRUE), FALSE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a AND b }", Args(TRUE, FALSE), FALSE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a AND b }", Args(FALSE, FALSE), FALSE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a OR b }", Args(TRUE, TRUE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a OR b }", Args(FALSE, TRUE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a OR b }", Args(TRUE, FALSE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a OR b }", Args(FALSE, FALSE), FALSE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| NOT a }", Args(TRUE, TRUE), FALSE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| NOT a }", Args(FALSE, TRUE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| NOT b }", Args(TRUE, FALSE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| NOT b }", Args(FALSE, FALSE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a XOR b }", Args(TRUE, FALSE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a XOR b }", Args(FALSE, TRUE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a XOR b }", Args(TRUE, FALSE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a XOR b }", Args(FALSE, FALSE), FALSE,typeof(LOGIC))
        // Normal logical operators
        TestMacro(mc, e"{|a,b| a .AND. b }", Args(TRUE, TRUE), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a .AND. b }", Args(FALSE, TRUE), FALSE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a .AND. b }", Args(TRUE, FALSE), FALSE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a .AND. b }", Args(FALSE, FALSE), FALSE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a .OR. b }", Args(TRUE, TRUE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a .OR. b }", Args(FALSE, TRUE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a .OR. b }", Args(TRUE, FALSE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a .OR. b }", Args(FALSE, FALSE), FALSE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| .NOT. a }", Args(TRUE, TRUE), FALSE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| .NOT. a }", Args(FALSE, TRUE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| .NOT. b }", Args(TRUE, FALSE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| .NOT. b }", Args(FALSE, FALSE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a .XOR. b }", Args(TRUE, FALSE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a .XOR. b }", Args(FALSE, TRUE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a .XOR. b }", Args(TRUE, FALSE), TRUE,typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a .XOR. b }", Args(FALSE, FALSE), FALSE,typeof(LOGIC))


        Console.WriteLine("Total pass: {0}/{1}", TotalSuccess, TotalTests)
        RETURN

    FUNCTION RunPerf(mc AS XSharp.Runtime.MacroCompiler, src AS STRING) AS VOID
        TestMacroCompiler(mc, src, 15, TRUE, FALSE)
        TestMacroCompiler(mc, src, 15, TRUE, TRUE)
        TestMacroCompiler(mc, src, 100000, FALSE, FALSE)
        TestMacroCompiler(mc, src, 100000, FALSE, TRUE)
        RETURN

    FUNCTION TestParse(mc AS XSharp.Runtime.MacroCompiler, src AS STRING, val AS STRING) AS LOGIC
        TotalTests += 1
        Console.Write("Test: '{0}' ", src)
        VAR res := mc:compiler:Parse(src):ToString()
        IF res = val
            TotalSuccess += 1
            Console.WriteLine("[OK]")
            RETURN TRUE
        ELSE
            TotalFails += 1
            Console.WriteLine("[FAIL] ({0} != {1})", res, val)
        END
        RETURN FALSE

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
/*        catch e as Exception
            TotalFails += 1
            Console.WriteLine("[FAIL] (Exception: {0})", e:Message)
            return false*/
        END

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
                VAR ast := mc:compiler:Parse(source)
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

