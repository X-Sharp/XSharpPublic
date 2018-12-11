USING System
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING XSharp.MacroCompiler
    USING XSharp.Runtime
FUNCTION U(u AS USUAL) AS USUAL
    RETURN u

FUNCTION R(r AS REAL8) AS REAL8
    RETURN r

FUNCTION I(i AS INT) AS INT
    RETURN i

FUNCTION A(i REF OBJECT) AS INT
    VAR v := i ASTYPE INT? DEFAULT 0
    i := 1000 + v
    RETURN v

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

CLASS testclass
    PUBLIC v1 AS INT
    PUBLIC v2 AS STRING

    STATIC PROPERTY sprop AS INT AUTO GET SET
    PROPERTY prop AS INT AUTO GET SET

    CONSTRUCTOR()
    CONSTRUCTOR(i AS INT)
        v1 := i
        prop := i

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

    OVERRIDE METHOD GetHashCode() AS INT
        RETURN SUPER:GetHashCode()
    OVERRIDE METHOD Equals(o AS OBJECT) AS LOGIC
        RETURN SELF == (teststruct)o
    OPERATOR ==(o1 AS teststruct, o2 AS teststruct) AS LOGIC
        RETURN o1:v1 == o2:v1 .AND. o1:v2 == o2:v2
    OPERATOR !=(o1 AS teststruct, o2 AS teststruct) AS LOGIC
        RETURN !(o1 == o2)
END STRUCT

GLOBAL tsi := teststruct{1} AS teststruct

GLOBAL tci := testclass{1} AS testclass

FUNCTION MyVarGet(name AS STRING) AS OBJECT
    RETURN "VarGet(" + name + ")"

FUNCTION MyVarPut(name AS STRING, VALUE AS OBJECT) AS OBJECT
    RETURN "VarPut(" + name +"):" + (STRING)VALUE

FUNCTION MyFieldGet(name AS STRING) AS OBJECT
    RETURN "FieldGet(" + name + ")"

FUNCTION MyFieldSet(name AS STRING, VALUE AS OBJECT) AS OBJECT
    RETURN "FieldSet(" + name +"):" + (STRING)VALUE

FUNCTION MyFieldGetWa(wa AS STRING, name AS STRING) AS OBJECT
    RETURN "FieldGet(" + wa + "," + name + ")"

FUNCTION MyFieldSetWa(wa AS STRING, name AS STRING, VALUE AS OBJECT) AS OBJECT
    RETURN "FieldSet(" + wa + "," + name +"):" + (STRING)VALUE

BEGIN NAMESPACE MacroCompilerTest
    USING XSharp.Runtime
    USING XSharp.MacroCompiler

	FUNCTION Start() AS VOID
	    SetMacroCompiler(typeof(XSharp.Runtime.MacroCompiler))

        ReportMemory("initial")
        VAR mc := CreateMacroCompiler()

        //ParseMacro(mc, e"{|a,b| +a[++b] += 100, a[2]}")

        //EvalMacro(mc, e"{|a,b| a[++b] += 100, a[2]}", {1,2,3}, 1)
        //EvalMacro(mc, e"{|a,b| 999999999999999999999999 + (-tsi+1)[2]}", {1,2,3}, 1)
        EvalMacro(mc, e"{|a,b| a $ b}", "est", "test")
        wait

        RunTests(mc)
        wait

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
        CATCH e AS XSharp.MacroCompiler.CompilationError
            Console.WriteLine("{0}",e:Message)
            RETURN NIL
        END

    GLOBAL TotalFails := 0 AS INT
    GLOBAL TotalTests := 0 AS INT
    GLOBAL TotalSuccess := 0 AS INT

    FUNCTION RunTests(mc AS XSharp.Runtime.MacroCompiler) AS VOID
        Console.WriteLine("Running tests ...")

        TestParse(mc, e"{|a,b| +a[++b] += 100, a[2]}", "{|a, b|((+a((++b)))+='100'), a('2')}")

        mc:Options:UndeclaredVariableResolution := VariableResolution.Error
        TestMacro(mc, e"{|a,b| testtest__() }", <OBJECT>{1,2,3}, NULL, NULL, ErrorCode.IdentifierNotFound)

        mc:Options:UndeclaredVariableResolution := VariableResolution.GenerateLocal
        TestMacro(mc, e"{|a| a() }", <OBJECT>{(@@Func<INT>){ => 1234}}, 1234, typeof(USUAL))
        TestMacro(mc, "#HELLo", <OBJECT>{}, #hello, typeof(SYMBOL))
        TestMacro(mc, "#HELLo + #World", <OBJECT>{}, #hello + #world, typeof(STRING))
        TestMacro(mc, e"#HELLo + \"world\"", <OBJECT>{}, #hello + "world", typeof(STRING))
        TestMacro(mc, e"\"Hello\" + #world", <OBJECT>{}, "Hello" + #world, typeof(STRING))
        TestMacro(mc, "U(12345)", <OBJECT>{}, 12345, typeof(USUAL))
        TestMacro(mc, "U(U(12345)-1)", <OBJECT>{}, 12344, typeof(USUAL))
        TestMacro(mc, "I(123+45)", <OBJECT>{}, 123+45, typeof(INT))
        TestMacro(mc, "R(123)", <OBJECT>{}, 123, typeof(REAL8))
        TestMacro(mc, "R(123.456)", <OBJECT>{}, 123.456, typeof(REAL8))
        TestMacro(mc, "U(123.456)", <OBJECT>{}, 123.456, typeof(USUAL))
        TestMacro(mc, "123.456", <OBJECT>{}, 123.456, typeof(FLOAT))
        TestMacro(mc, "123.456s", <OBJECT>{}, 123.456s, typeof(REAL4))
        TestMacro(mc, "123.456d", <OBJECT>{}, 123.456d, typeof(REAL8))
        TestMacro(mc, "123.450m", <OBJECT>{}, 123.450m, typeof(decimal))
        TestMacro(mc, "123.450m+321", <OBJECT>{}, 444.450m, typeof(decimal))
        TestMacro(mc, "123.450m+321.05", <OBJECT>{}, 444.500m, typeof(decimal))
        TestMacro(mc, "{|a,b,c|a := b := 1343+1}", <OBJECT>{}, 1343+1, typeof(INT))
        TestMacro(mc, "{|a,b,c|}", <OBJECT>{}, NULL, NULL)
        TestMacro(mc, "{|a,b,c|1234}", <OBJECT>{}, 1234, typeof(INT))
        TestMacro(mc, "1234", <OBJECT>{}, 1234, typeof(INT))
        TestMacro(mc, "12 == 12", <OBJECT>{}, TRUE, typeof(LOGIC))
        TestMacro(mc, "", <OBJECT>{}, NULL, NULL)
        TestMacro(mc, "2018.12.31", <OBJECT>{}, 2018.12.31, typeof(DATE))
        TestMacro(mc, "2018.1.1", <OBJECT>{}, 2018.1.1, typeof(DATE))
        TestMacro(mc, "2018.12.31 == 2018.12.31", <OBJECT>{}, TRUE, typeof(LOGIC))
        TestMacro(mc, "2018.12.31 = 2018.12.31", <OBJECT>{}, TRUE, typeof(LOGIC))
        TestMacro(mc, "2018.12.31 = 2018.1.1", <OBJECT>{}, FALSE, typeof(LOGIC))
        TestMacro(mc, "2018.12.31 != 2018.12.31", <OBJECT>{}, FALSE, typeof(LOGIC))
        TestMacro(mc, "2018.12.31 != 2018.1.1", <OBJECT>{}, TRUE, typeof(LOGIC))
        TestMacro(mc, "null", <OBJECT>{}, NULL, NULL)
        TestMacro(mc, "null_object", <OBJECT>{}, NULL_OBJECT, NULL)
        TestMacro(mc, "null_string", <OBJECT>{}, NULL_STRING, NULL)
        TestMacro(mc, "null_psz = psz._NULL_PSZ", <OBJECT>{}, TRUE, typeof(LOGIC))
        TestMacro(mc, "null_symbol", <OBJECT>{}, NULL_SYMBOL, typeof(SYMBOL))
        TestMacro(mc, "null_date", <OBJECT>{}, NULL_DATE, typeof(DATE))
        TestMacro(mc, "null_codeblock", <OBJECT>{}, NULL_CODEBLOCK, NULL)
        TestMacro(mc, "null_ptr", <OBJECT>{}, NULL_PTR, NULL)
        TestMacro(mc, "{|a,b,c|a := b := 1343, c := a + 1, a+b-c/2}", <OBJECT>{}, 1343+1343-(1343+1)/2, typeof(USUAL))
        TestMacro(mc, "{|a|a := 1343, a += 1}", <OBJECT>{}, 1343+1, typeof(USUAL))
        TestMacro(mc, "{|a|a := -1343, a := -a}", <OBJECT>{}, 1343, typeof(USUAL))
        TestMacro(mc, "{|a|a := 8, ++a, ++a}", <OBJECT>{123}, 10, typeof(USUAL))
        TestMacro(mc, "{|a|a := 8, ++a, a++, a++}", <OBJECT>{123}, 10, typeof(USUAL))
        TestMacro(mc, "{|a|++a, a++, a++}", <OBJECT>{8}, 10, typeof(USUAL))
        TestMacro(mc, "{|a| a++, U(a++), a++}", <OBJECT>{8}, 10, typeof(USUAL))
        TestMacro(mc, e"{|a| a := \"abc\" + \"def\"}", <OBJECT>{8}, "abcdef", typeof(STRING))
        TestMacro(mc, e"{|a| \"abc\" == \"def\"}", <OBJECT>{8}, FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a| \"abc\" = \"abc\"}", <OBJECT>{8}, TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a| \"abc\" != \"abc\"}", <OBJECT>{8}, FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a| a := \"abc\", a == \"abc\"}", <OBJECT>{8}, TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a| a := \"abc\", a + \"def\"}", <OBJECT>{8}, "abcdef", typeof(STRING))
        TestMacro(mc, e"{|a| 0 == 0}", <OBJECT>{8}, TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a| 0 != 0}", <OBJECT>{8}, FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a| (0 > 1) .and. (0 < 1) }", <OBJECT>{8}, FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a| a := \"qwerty\", a:Length }", <OBJECT>{8}, 6, typeof(USUAL))
        TestMacro(mc, e"{|a| a := default(int) }", <OBJECT>{8}, 0, typeof(INT))
        TestMacro(mc, e"{|a| a := default(string) }", <OBJECT>{8}, NULL, NULL)
        TestMacro(mc, e"{|a| a := default(usual) }", <OBJECT>{8}, NIL, typeof(USUAL))
        TestMacro(mc, e"{|a| a := U(1234+1), a }", <OBJECT>{8}, 1234+1, typeof(USUAL))
        TestMacro(mc, e"{|a| UU := U(1234+1), UU }", <OBJECT>{8}, 1234+1, typeof(USUAL))
        TestMacro(mc, e"{|a| a := \"abcdef\", a:ToUpperInvariant() }", <OBJECT>{8}, "ABCDEF", typeof(USUAL))
        TestMacro(mc, e"{|a| a := NIL }", <OBJECT>{8}, NIL, typeof(USUAL))
        TestMacro(mc, e"{|| I3(4,4,4) }", <OBJECT>{}, 12, typeof(USUAL))
        TestMacro(mc, e"{|a| a := I3(4,4,) }", <OBJECT>{}, 11, typeof(USUAL))
        TestMacro(mc, e"{|a| a := I3(4,,4) }", <OBJECT>{}, 10, typeof(USUAL))
        TestMacro(mc, e"{|a| a := I3(,4,4) }", <OBJECT>{}, 9, typeof(USUAL))
        TestMacro(mc, e"{|a| a := I3(,,) }", <OBJECT>{}, 6, typeof(USUAL))
        TestMacro(mc, e"{|a| a := I3(4,4) }", <OBJECT>{}, 11, typeof(USUAL))
        TestMacro(mc, e"{|a| a := I3(4) }", <OBJECT>{}, 9, typeof(USUAL))
        TestMacro(mc, e"{|a| a := I3() }", <OBJECT>{}, 6, typeof(USUAL))
        TestMacro(mc, e"{|a| a := I0() }", <OBJECT>{}, 123, typeof(INT))
        TestMacro(mc, e"{|a| a := CC(1,2,3) }", <OBJECT>{}, 6, typeof(USUAL))
        TestMacro(mc, e"{|a| a := CC(1,2,3,4) }", <OBJECT>{}, 6, typeof(USUAL))
        TestMacro(mc, e"{|a| a := CC() }", <OBJECT>{}, 0, typeof(USUAL))
        TestMacro(mc, e"{|a| a := CC(1,2) }", <OBJECT>{}, 3, typeof(USUAL))
        TestMacro(mc, e"{|a| a := CC(,1,2) }", <OBJECT>{}, 3, typeof(USUAL))
        TestMacro(mc, e"{|a| a := CC(1,,2) }", <OBJECT>{}, 3, typeof(USUAL))
        TestMacro(mc, e"{|a| a := U({1,2,3}) }", <OBJECT>{}, {1,2,3}, typeof(USUAL))
        TestMacro(mc, e"{|a| a := U({1,,2,,}) }", <OBJECT>{}, {1,NIL,2,NIL,NIL}, typeof(USUAL))
        TestMacro(mc, e"{|a| a := <INT>{1,2,3} }", <OBJECT>{}, <INT>{1,2,3}, typeof(INT[]))
        TestMacro(mc, e"object{}", <OBJECT>{}, OBJECT{}, typeof(OBJECT))
        TestMacro(mc, e"teststruct{12}", <OBJECT>{}, teststruct{12}, typeof(teststruct))
        TestMacro(mc, e"teststruct{}", <OBJECT>{}, teststruct{}, typeof(teststruct))
        TestMacro(mc, e"testclass{}", <OBJECT>{}, testclass{}, typeof(testclass))
        TestMacro(mc, e"testclass{23}", <OBJECT>{}, testclass{23}, typeof(testclass))
        TestMacro(mc, e"testclassdc{}", <OBJECT>{}, testclassdc{}, typeof(testclassdc))
        TestMacro(mc, e"int{}", <OBJECT>{}, 0, typeof(INT))
        TestMacro(mc, e"{|z| A(z) }", <OBJECT>{123}, 123, typeof(int32))
        TestMacro(mc, e"{|z| A(z), z }", <OBJECT>{123}, 1123, typeof(int32))
        TestMacro(mc, e"{|a| testclass.sprop := 555, a := ++testclass.sprop }", <OBJECT>{}, 556, typeof(int32))
        TestMacro(mc, e"{|a| testclass.sprop := a, a := ++testclass.sprop }", <OBJECT>{55}, 56, typeof(int32))
        TestMacro(mc, e"{|a| teststruct.sprop := 555, a := ++teststruct.sprop }", <OBJECT>{}, 556, typeof(int32))
        TestMacro(mc, e"{|a| teststruct.sprop := a, a := ++teststruct.sprop }", <OBJECT>{55}, 56, typeof(int32))
        TestMacro(mc, e"{|a| a := testclass{}, a:prop }", <OBJECT>{}, 0, typeof(USUAL))
        TestMacro(mc, e"{|a| a := testclass{222}, a:prop }", <OBJECT>{}, 222, typeof(USUAL))
        TestMacro(mc, e"{|a| a := teststruct{}, a:prop }", <OBJECT>{}, 0, typeof(USUAL))
        TestMacro(mc, e"{|a| a := teststruct{222}, a:prop }", <OBJECT>{}, 222, typeof(USUAL))
        TestMacro(mc, e"{|a| a := testclass{}, a:v1 := 1 }", <OBJECT>{}, 1, typeof(USUAL))
        TestMacro(mc, e"{|a| a := testclass{222}, a:v1 }", <OBJECT>{}, 222, typeof(USUAL))
        TestMacro(mc, e"{|a| a := teststruct{}, a:v1 := 1 }", <OBJECT>{}, 1, typeof(USUAL))
        TestMacro(mc, e"{|a| a := teststruct{222}, a:v1 }", <OBJECT>{}, 222, typeof(USUAL))
        TestMacro(mc, e"{|a| a := testclass{}, a:prop := 111 }", <OBJECT>{}, 111, typeof(USUAL))
        TestMacro(mc, e"{|a,b| b := testclass{}, b:prop := a, ++b:prop }", <OBJECT>{55}, 56, typeof(USUAL))
        TestMacro(mc, e"{|| tsi:v1 }", <OBJECT>{}, 1, typeof(USUAL))
        TestMacro(mc, e"{|| ++tsi:v1 }", <OBJECT>{}, 2, typeof(USUAL))
//        TestMacro(mc, e"{|| tsi:v1 := 10, tsi:v1 }", <OBJECT>{}, 10, typeof(usual)) // FAIL because tsi is boxed by value for IVarPut()
        TestMacro(mc, e"{|| ++tsi:prop }", <OBJECT>{}, 2, typeof(USUAL))
        TestMacro(mc, e"{|| tci:v1 }", <OBJECT>{}, 1, typeof(USUAL))
        TestMacro(mc, e"{|| ++tci:v1 }", <OBJECT>{}, 2, typeof(USUAL))
        TestMacro(mc, e"{|| ++tci:prop }", <OBJECT>{}, 2, typeof(USUAL))
        TestMacro(mc, e"{|| tci:v1 := 10, tci:v1++, tci:v1 }", <OBJECT>{}, 11, typeof(USUAL))
        TestMacro(mc, e"{|a| IIF(a>10,123,1.23) }", <OBJECT>{100}, 123, typeof(FLOAT))
        TestMacro(mc, e"{|a| IIF(a>10,123,1.23) }", <OBJECT>{1}, 1.23, typeof(FLOAT))
        TestMacro(mc, e"{|a| IIF(a>10,1) }", <OBJECT>{100}, 1, typeof(USUAL))
        TestMacro(mc, e"{|a| IIF(a>10,,1) }", <OBJECT>{100}, NIL, typeof(USUAL))
        TestMacro(mc, e"{|a| (float)++a/2 }", <OBJECT>{2}, 1.5, typeof(FLOAT))
        TestMacro(mc, e"{|a| a := {1,2,3,4}, a[1] := 10, a[1] + a[2] }", <OBJECT>{}, 12, typeof(USUAL))
        TestMacro(mc, e"{|a| a := {1,2,3,4}, a[1] += 10, a[1] }", <OBJECT>{}, 11, typeof(USUAL))
        TestMacro(mc, "{|a|a := 8, a := 8**a}", <OBJECT>{123}, 16777216, typeof(FLOAT))
        TestMacro(mc, "I((int)123.456)", <OBJECT>{}, 123, typeof(INT))
        TestMacro(mc, "{|a| b := 8, c := b**a, c}", <OBJECT>{8}, 16777216, typeof(USUAL))
        TestMacro(mc, "{|a,b,c|a.and.b.or..not.c}", <OBJECT>{TRUE,FALSE,TRUE}, FALSE, typeof(LOGIC))
        TestMacro(mc, "{|a| a := U({1,2,3", <OBJECT>{}, {1,2,3}, typeof(USUAL))
//        TestMacro(mc, e"{|| _FIELD->NIKOS}", <OBJECT>{}, nil, typeof(usual))
//        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS}", <OBJECT>{}, nil, typeof(usual))
//        TestMacro(mc, e"{|| BASE->NIKOS}", <OBJECT>{}, nil, typeof(usual))
//        TestMacro(mc, e"{|| _FIELD->NIKOS := 123}", <OBJECT>{}, 123, typeof(usual))
//        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS := 123}", <OBJECT>{}, 123, typeof(usual))
//        TestMacro(mc, e"{|| BASE->NIKOS := 123}", <OBJECT>{}, 123, typeof(usual))
        TestMacro(mc, e"{|a,b| a[++b] += 100, a[2]}", <OBJECT>{{1,2,3}, 1}, 102, typeof(USUAL))
        TestMacro(mc, e"_chr(65)", <OBJECT>{}, 65, typeof(CHAR))
        TestMacro(mc, e"chr(65)", <OBJECT>{}, 65, typeof(CHAR))
        TestMacro(mc, e"char(65)", <OBJECT>{}, 65, typeof(CHAR))
        TestMacro(mc, e"slen(\"hello\")", <OBJECT>{}, 5, typeof(DWORD))
        TestMacro(mc, e"{|v| v[2,1,2,1,1] }", <OBJECT>{ {{}, {{ "1_78", {{ 'DATEI_1', 'C', 100, 0,'Anhang 1','Anhang1' }}, NIL, NIL }}} },"DATEI_1", typeof(USUAL))
        TestMacro(mc, e"{|v| v[2,1,2,1,1] := 'TEST', v[2,1,2,1,1] }", <OBJECT>{ {{}, {{ "1_78", {{ 'DATEI_1', 'C', 100, 0,'Anhang 1','Anhang1' }}, NIL, NIL }}} },"TEST", typeof(USUAL)) 
        TestMacro(mc, e"{|a| a[2,2,2,2,2] := 12, a[2,2,2,2,2] }", <OBJECT>{ {1,{1,{1,{1,{1, 3}}}}} }, 12 , typeof(USUAL)) // FAIL - due to ARRAY:__SetElement() bug
        TestMacro(mc, e"{|a| a:ToString() }", <OBJECT>{8}, "8", typeof(STRING)) // FAIL - String:ToString() is overloaded!
        TestMacro(mc, e"{|a,b| a $ b}", <OBJECT>{"est", "test"}, TRUE, typeof(boolean))
        TestMacro(mc, e"{|a,b| a $ b}", <OBJECT>{"test", "est"}, FALSE, typeof(boolean))

        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsField
        TestMacro(mc, e"{|| NIKOS}", <OBJECT>{}, NIL, typeof(USUAL))
        TestMacro(mc, e"{|| NIKOS := 123}", <OBJECT>{}, 123, typeof(USUAL))

        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsFieldOrMemvar
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___VarGet, "MyVarGet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___VarPut, "MyVarPut")
        TestMacro(mc, e"{|| NIKOS}", <OBJECT>{}, "VarGet(NIKOS)", typeof(USUAL))
        TestMacro(mc, e"{|| NIKOS := \"123\"}", <OBJECT>{}, "VarPut(NIKOS):123", typeof(USUAL))

        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsField
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGet, "MyFieldGet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSet, "MyFieldSet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGetWa, "MyFieldGetWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSetWa, "MyFieldSetWa")
        TestMacro(mc, e"{|| NIKOS}", <OBJECT>{}, "FieldGet(NIKOS)", typeof(USUAL))
        TestMacro(mc, e"{|| NIKOS := \"123\"}", <OBJECT>{}, "FieldSet(NIKOS):123", typeof(USUAL))
        TestMacro(mc, e"{|| _FIELD->NIKOS}", <OBJECT>{}, "FieldGet(NIKOS)", typeof(USUAL))
        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS}", <OBJECT>{}, "FieldGet(BASE,NIKOS)", typeof(USUAL))
        TestMacro(mc, e"{|| BASE->NIKOS}", <OBJECT>{}, "FieldGet(BASE,NIKOS)", typeof(USUAL))
        TestMacro(mc, e"{|| _FIELD->NIKOS := \"123\"}", <OBJECT>{}, "FieldSet(NIKOS):123", typeof(USUAL))
        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS := \"123\"}", <OBJECT>{}, "FieldSet(BASE,NIKOS):123", typeof(USUAL))
        TestMacro(mc, e"{|| BASE->NIKOS := \"123\"}", <OBJECT>{}, "FieldSet(BASE,NIKOS):123", typeof(USUAL))

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
            //Console.Write("Test: '{0}' ", src)
            VAR cb := mc:Compile(src)
            VAR res := cb:EvalBlock(args)
            LOCAL match AS LOGIC
            IF IsArray(expect)
                match := ALen(expect) = ALen((USUAL)res)
                FOR VAR i := 1 TO ALen(expect)
                    IF expect[i] != ((USUAL)res)[i]
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
            IF (ec == ErrorCode.NoError) .AND. (match) .AND. ((t == NULL) || (t == res?:GetType()))
                TotalSuccess += 1
                //Console.Write("Test: '{0}' ", src)
                //Console.WriteLine("[OK]")
                RETURN TRUE
            ELSE
                TotalFails += 1
                Console.Write("Test: '{0}' ", src)
                Console.WriteLine("[FAIL] (res = {0}, type = {1}, no error)", res, res?:GetType())
            END
            RETURN FALSE
        CATCH e AS XSharp.MacroCompiler.CompilationError
            IF e:@@Code == ec
                TotalSuccess += 1
                //Console.Write("Test: '{0}' ", src)
                //Console.WriteLine("[OK]")
                RETURN TRUE
            ELSE
                TotalFails += 1
                Console.Write("Test: '{0}' ", src)
                Console.WriteLine("[FAIL] ({0})", e:Message)
            END
            RETURN FALSE
        CATCH e AS Exception
            TotalFails += 1
            Console.Write("Test: '{0}' ", src)
            Console.WriteLine("[FAIL] ({0})", e:Message)
            RETURN FALSE
        END

    FUNCTION CreateMacroCompiler() AS XSharp.Runtime.MacroCompiler
        Console.WriteLine("Creating macro compiler ...")

        VAR m := GC:GetTotalMemory(TRUE)
        VAR t := DateTime.Now

        VAR mc := XSharp.Runtime.MacroCompiler{}

        VAR dt := DateTime.Now - t
        t += dt

        Console.WriteLine("  Completed in {0}", dt)
        Console.WriteLine("  Memory: +{0} bytes", GC.GetTotalMemory(FALSE) - m)
        Console.WriteLine()
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


