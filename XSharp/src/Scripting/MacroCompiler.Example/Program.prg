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

GLOBAL tsi := teststruct{1} AS teststruct

GLOBAL tci := testclass{1} AS testclass

global wag := "" as string

FUNCTION MyVarGet(name AS STRING) AS USUAL
    RETURN wag + "VarGet(" + name + ")"

FUNCTION MyVarPut(name AS STRING, VALUE AS USUAL) AS USUAL
    RETURN wag + "VarPut(" + name +"):" + VALUE:ToString()

FUNCTION MyFieldGet(name AS STRING) AS USUAL
    RETURN wag + "FieldGet(" + name + ")"

FUNCTION MyFieldSet(name AS STRING, VALUE AS USUAL) AS USUAL
    RETURN wag + "FieldSet(" + name +"):" + VALUE:ToString()

FUNCTION MyFieldGetWa(wa AS STRING, name AS STRING) AS USUAL
    RETURN "FieldGet(" + wa + "," + name + ")"

FUNCTION MyFieldSetWa(wa AS STRING, name AS STRING, VALUE AS USUAL) AS USUAL
    RETURN "FieldSet(" + wa + "," + name +"):" + VALUE:ToString()

FUNCTION MyPushWa(wa as usual) as void
    wag := wa + "->"
    return

FUNCTION MyPopWa() as void
    wag := ""
    return

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

function testDef(cFormularName AS STRING, uDataSource := NIL AS USUAL, ;
uVorlage := "" AS USUAL, cOeffnenModus := "F9" AS USUAL, uParameter := NIL AS USUAL, ;
oDataContext := NULL AS OBJECT, cWinMode := NULL AS USUAL, oProtype := NULL AS OBJECT, ;
oDBVorlage := NULL AS OBJECT ) AS USUAL STRICT
    return "testDef"

FUNC TestInt32(n AS INT) AS INT
RETURN n
FUNC TestDWord(n AS DWORD) AS DWORD
RETURN n

func testRet(x as string) as string
return x

global Error := 321 as int

global ErrorLevel := 1 as int

class ErrString
    static V := 333 as int
end class

BEGIN NAMESPACE MacroCompilerTest

	FUNCTION Start() AS VOID
        //StartTest()
        //return

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
        CATCH e AS CompilationError
            Console.WriteLine("{0}",e:Message)
            RETURN NIL
        END

    GLOBAL TotalFails := 0 AS INT
    GLOBAL TotalTests := 0 AS INT
    GLOBAL TotalSuccess := 0 AS INT

    FUNCTION RunTests(mc AS XSharp.Runtime.MacroCompiler) AS VOID
        Console.WriteLine("Running tests ...")

        TestParse(mc, e"{|a,b| +a[++b] += 100, a[2]}", "{|a, b|((+a((++b)))+='100'), a('2')}")
        TestMacro(mc, e"{|a,b| asdgfafd(123) }", Args(), NULL, NULL,ErrorCode.NotAMethod)

        mc:Options:UndeclaredVariableResolution := VariableResolution.Error
        TestMacro(mc, e"{|a,b| testtest__() }", Args(1,2,3), NULL, NULL, ErrorCode.IdentifierNotFound)

        mc:Options:UndeclaredVariableResolution := VariableResolution.GenerateLocal
        TestMacro(mc, e"{|a| a() }", Args((@@Func<INT>){ => 1234}), 1234, typeof(INT))
        TestMacro(mc, "#HELLo", Args(), #hello, typeof(SYMBOL))
        TestMacro(mc, "#HELLo + #World", Args(), #hello + #world, typeof(STRING))
        TestMacro(mc, e"#HELLo + \"world\"", Args(), #hello + "world", typeof(STRING))
        TestMacro(mc, e"\"Hello\" + #world", Args(), "Hello" + #world, typeof(STRING))
        TestMacro(mc, "U(12345)", Args(), 12345, typeof(INT))
        TestMacro(mc, "U(U(12345)-1)", Args(), 12344, typeof(INT))
        TestMacro(mc, "I(123+45)", Args(), 123+45, typeof(INT))
        TestMacro(mc, "R(123)", Args(), 123, typeof(REAL8))
        TestMacro(mc, "R(123.456)", Args(), 123.456, typeof(REAL8))
        TestMacro(mc, "U(123.456)", Args(), 123.456, typeof(FLOAT))
        TestMacro(mc, "123.456", Args(), 123.456, typeof(FLOAT))
        TestMacro(mc, "123.456s", Args(), 123.456s, typeof(REAL4))
        TestMacro(mc, "123.456d", Args(), 123.456d, typeof(REAL8))
        TestMacro(mc, "123.450m", Args(), 123.450m, typeof(decimal))
        TestMacro(mc, "123.450m+321", Args(), 444.450m, typeof(decimal))
        TestMacro(mc, "123.450m+321.05", Args(), 444.500m, typeof(decimal))
        TestMacro(mc, "{|a,b,c|a := b := 1343+1}", Args(), 1343+1, typeof(INT))
        TestMacro(mc, "{|a,b,c|}", Args(), NULL, typeof(OBJECT))
        TestMacro(mc, "{|a,b,c|1234}", Args(), 1234, typeof(INT))
        TestMacro(mc, "1234", Args(), 1234, typeof(INT))
        TestMacro(mc, "12 == 12", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, "", Args(), NULL, typeof(OBJECT))
        TestMacro(mc, "2018.12.31", Args(), 2018.12.31, typeof(DATE))
        TestMacro(mc, "2018.1.1", Args(), 2018.1.1, typeof(DATE))
        TestMacro(mc, "2018.12.31 == 2018.12.31", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, "2018.12.31 = 2018.12.31", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, "2018.12.31 = 2018.1.1", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, "2018.12.31 != 2018.12.31", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, "2018.12.31 != 2018.1.1", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, "null", Args(), NULL, typeof(OBJECT))
        TestMacro(mc, "null_object", Args(), NULL_OBJECT, NULL)
        TestMacro(mc, "null_string", Args(), NULL_STRING, NULL)
        TestMacro(mc, "null_psz = psz._NULL_PSZ", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, "null_symbol", Args(), NULL_SYMBOL, typeof(SYMBOL))
        TestMacro(mc, "null_date", Args(), NULL_DATE, typeof(DATE))
        TestMacro(mc, "null_codeblock", Args(), NULL_CODEBLOCK, NULL)
        TestMacro(mc, "null_ptr", Args(), NULL_PTR, NULL)
        TestMacro(mc, "{|a,b,c|a := b := 1343, c := a + 1, a+b-c/2}", Args(), 1343+1343-(1343+1)/2, typeof(INT))
        TestMacro(mc, "{|a|a := 1343, a += 1}", Args(), 1343+1, typeof(INT))
        TestMacro(mc, "{|a|a := -1343, a := -a}", Args(), 1343, typeof(INT))
        TestMacro(mc, "{|a|a := 8, ++a, ++a}", Args(123), 10, typeof(INT))
        TestMacro(mc, "{|a|a := 8, ++a, a++, a++}", Args(123), 10, typeof(INT))
        TestMacro(mc, "{|a|++a, a++, a++}", Args(8), 10, typeof(INT))
        TestMacro(mc, "{|a| a++, U(a++), a++}", Args(8), 10, typeof(INT))
        TestMacro(mc, e"{|a| a := \"abc\" + \"def\"}", Args(8), "abcdef", typeof(STRING))
        TestMacro(mc, e"{|a| \"abc\" == \"def\"}", Args(8), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a| \"abc\" = \"abc\"}", Args(8), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a| \"abc\" != \"abc\"}", Args(8), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a| a := \"abc\", a == \"abc\"}", Args(8), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a| a := \"abc\", a + \"def\"}", Args(8), "abcdef", typeof(STRING))
        TestMacro(mc, e"{|a| 0 == 0}", Args(8), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a| 0 != 0}", Args(8), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a| (0 > 1) .and. (0 < 1) }", Args(8), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a| a := \"qwerty\", a:Length }", Args(8), 6, typeof(INT))
        TestMacro(mc, e"{|a| a := default(int) }", Args(8), 0, typeof(INT))
        TestMacro(mc, e"{|a| a := default(string) }", Args(8), NULL, typeof(OBJECT))
        TestMacro(mc, e"{|a| a := default(usual) }", Args(8), NULL, typeof(OBJECT))
        TestMacro(mc, e"{|a| a := U(1234+1), a }", Args(8), 1234+1, typeof(INT))
        TestMacro(mc, e"{|a| UU := U(1234+1), UU }", Args(8), 1234+1, typeof(INT))
        TestMacro(mc, e"{|a| a := \"abcdef\", a:ToUpperInvariant() }", Args(8), "ABCDEF", typeof(STRING))
        TestMacro(mc, e"{|a| a := NIL }", Args(8), NULL, typeof(OBJECT))
        TestMacro(mc, e"{|| I3(4,4,4) }", Args(), 12, typeof(INT))
        TestMacro(mc, e"{|a| a := I3(4,4,) }", Args(), 11, typeof(INT))
        TestMacro(mc, e"{|a| a := I3(4,,4) }", Args(), 10, typeof(INT))
        TestMacro(mc, e"{|a| a := I3(,4,4) }", Args(), 9, typeof(INT))
        TestMacro(mc, e"{|a| a := I3(,,) }", Args(), 6, typeof(INT))
        TestMacro(mc, e"{|a| a := I3(4,4) }", Args(), 11, typeof(INT))
        TestMacro(mc, e"{|a| a := I3(4) }", Args(), 9, typeof(INT))
        TestMacro(mc, e"{|a| a := I3() }", Args(), 6, typeof(INT))
        TestMacro(mc, e"{|a| a := I0() }", Args(), 123, typeof(INT))
        TestMacro(mc, e"{|a| a := CC(1,2,3) }", Args(), 6, typeof(INT))
        TestMacro(mc, e"{|a| a := CC(1,2,3,4) }", Args(), 6, typeof(INT))
        TestMacro(mc, e"{|a| a := CC() }", Args(), 0, typeof(INT))
        TestMacro(mc, e"{|a| a := CC(1,2) }", Args(), 3, typeof(INT))
        TestMacro(mc, e"{|a| a := CC(,1,2) }", Args(), 3, typeof(INT))
        TestMacro(mc, e"{|a| a := CC(1,,2) }", Args(), 3, typeof(INT))
        TestMacro(mc, e"{|a| a := U({1,2,3}) }", Args(), {1,2,3}, typeof(ARRAY))
        TestMacro(mc, e"{|a| a := U({1,,2,,}) }", Args(), {1,NIL,2,NIL,NIL}, typeof(ARRAY))
        TestMacro(mc, e"{|a| a := <INT>{1,2,3} }", Args(), <INT>{1,2,3}, typeof(INT[]))
        TestMacro(mc, e"object{}", Args(), OBJECT{}, typeof(OBJECT))
        TestMacro(mc, e"teststruct{12}", Args(), teststruct{12}, typeof(teststruct))
        TestMacro(mc, e"teststruct{}", Args(), teststruct{}, typeof(teststruct))
        TestMacro(mc, e"testclass{}", Args(), testclass{}, typeof(testclass))
        TestMacro(mc, e"testclass{23}", Args(), testclass{23}, typeof(testclass))
        TestMacro(mc, e"testclassdc{}", Args(), testclassdc{}, typeof(testclassdc))
        TestMacro(mc, e"int{}", Args(), 0, typeof(INT))
        TestMacro(mc, e"{|z| A(z) }", Args(123), 123, typeof(int32))
        TestMacro(mc, e"{|z| A(z), z }", Args(123), 1123, typeof(int32))
        TestMacro(mc, e"{|a| testclass.sprop := 555, a := ++testclass.sprop }", Args(), 556, typeof(int32))
        TestMacro(mc, e"{|a| testclass.sprop := a, a := ++testclass.sprop }", Args(55), 56, typeof(int32))
        TestMacro(mc, e"{|a| teststruct.sprop := 555, a := ++teststruct.sprop }", Args(), 556, typeof(int32))
        TestMacro(mc, e"{|a| teststruct.sprop := a, a := ++teststruct.sprop }", Args(55), 56, typeof(int32))
        TestMacro(mc, e"{|a| a := testclass{}, a:prop }", Args(), 0, typeof(INT))
        TestMacro(mc, e"{|a| a := testclass{222}, a:prop }", Args(), 222, typeof(INT))
        TestMacro(mc, e"{|a| a := teststruct{}, a:prop }", Args(), 0, typeof(INT))
        TestMacro(mc, e"{|a| a := teststruct{222}, a:prop }", Args(), 222, typeof(INT))
        TestMacro(mc, e"{|a| a := testclass{}, a:v1 := 1 }", Args(), 1, typeof(INT))
        TestMacro(mc, e"{|a| a := testclass{222}, a:v1 }", Args(), 222, typeof(INT))
        TestMacro(mc, e"{|a| a := teststruct{}, a:v1 := 1 }", Args(), 1, typeof(INT))
        TestMacro(mc, e"{|a| a := teststruct{222}, a:v1 }", Args(), 222, typeof(INT))
        TestMacro(mc, e"{|a| a := testclass{}, a:prop := 111 }", Args(), 111, typeof(INT))
        TestMacro(mc, e"{|a,b| b := testclass{}, b:prop := a, ++b:prop }", Args(55), 56, typeof(INT))
        TestMacro(mc, e"{|| tsi:v1 }", Args(), 1, typeof(INT))
        TestMacro(mc, e"{|| ++tsi:v1 }", Args(), 2, typeof(INT))
//        TestMacro(mc, e"{|| tsi:v1 := 10, tsi:v1 }", Args(), 10, typeof(int)) // FAIL because tsi is boxed by value for IVarPut()
        TestMacro(mc, e"{|| ++tsi:prop }", Args(), 2, typeof(INT))
        TestMacro(mc, e"{|| tci:v1 }", Args(), 1, typeof(INT))
        TestMacro(mc, e"{|| ++tci:v1 }", Args(), 2, typeof(INT))
        TestMacro(mc, e"{|| ++tci:prop }", Args(), 2, typeof(INT))
        TestMacro(mc, e"{|| tci:v1 := 10, tci:v1++, tci:v1 }", Args(), 11, typeof(INT))
        TestMacro(mc, e"{|a| IIF(a>10,123,1.23) }", Args(100), 123, typeof(FLOAT))
        TestMacro(mc, e"{|a| IIF(a>10,123,1.23) }", Args(1), 1.23, typeof(FLOAT))
        TestMacro(mc, e"{|a| IIF(a>10,1) }", Args(100), 1, typeof(INT))
        TestMacro(mc, e"{|a| IIF(a>10,,1) }", Args(100), NULL, typeof(OBJECT))
        TestMacro(mc, e"{|a| (float)++a/2 }", Args(2), 1.5, typeof(FLOAT))
        TestMacro(mc, e"{|a| a := {1,2,3,4}, a[1] := 10, a[1] + a[2] }", Args(), 12, typeof(INT))
        TestMacro(mc, e"{|a| a := {1,2,3,4}, a[1] += 10, a[1] }", Args(), 11, typeof(INT))
        TestMacro(mc, "{|a|a := 8, a := 8**a}", Args(123), 16777216, typeof(FLOAT))
        TestMacro(mc, "I((int)123.456)", Args(), 123, typeof(INT))
        TestMacro(mc, "{|a| b := 8, c := b**a, c}", Args(8), 16777216, typeof(FLOAT))
        TestMacro(mc, "{|a,b,c|a.and.b.or..not.c}", Args(TRUE,FALSE,TRUE), FALSE, typeof(LOGIC))
        TestMacro(mc, "{|a| a := U({1,2,3", Args(), {1,2,3}, typeof(ARRAY))
//        TestMacro(mc, e"{|| _FIELD->NIKOS}", Args(), nil, typeof(object))
//        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS}", Args(), nil, typeof(object))
//        TestMacro(mc, e"{|| BASE->NIKOS}", Args(), nil, typeof(object))
//        TestMacro(mc, e"{|| _FIELD->NIKOS := 123}", Args(), 123, typeof(object))
//        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS := 123}", Args(), 123, typeof(object))
//        TestMacro(mc, e"{|| BASE->NIKOS := 123}", Args(), 123, typeof(object))
        TestMacro(mc, e"{|a,b| a[++b] += 100, a[2]}", Args({1,2,3}, 1), 102, typeof(INT))
        TestMacro(mc, e"_chr(65)", Args(), Chr(65), typeof(STRING))
        TestMacro(mc, e"chr(65)", Args(), Chr(65), typeof(STRING))
        TestMacro(mc, e"char(65)", Args(), 65, typeof(CHAR))
        TestMacro(mc, e"slen(\"hello\")", Args(), 5, typeof(DWORD))
        TestMacro(mc, e"{|v| v[2] }", Args( <OBJECT>{ { 'C', 100, 0} } ),100, typeof(INT))
        TestMacro(mc, e"{|v| v[2,1,2,1,1] }", <OBJECT>{ {{}, {{ "1_78", {{ 'DATEI_1', 'C', 100, 0,'Anhang 1','Anhang1' }}, NIL, NIL }}} },"DATEI_1", typeof(STRING))
//        TestMacro(mc, e"{|v| v[2,1,2,1,1] := 'TEST', v[2,1,2,1,1] }", <object>{ {{}, {{ "1_78", {{ 'DATEI_1', 'C', 100, 0,'Anhang 1','Anhang1' }}, nil, nil }}} },"DATEI_1", typeof(string)) // FAIL - due to ARRAY:__SetElement() bug
//        TestMacro(mc, e"{|a| a[2,2,2,2,2] := 12, a[2,2,2,2,2] }", <object>{ {1,{1,{1,{1,{1, 3}}}}} }, 12 , typeof(int)) // FAIL - due to ARRAY:__SetElement() bug
        TestMacro(mc, e"{|a| a:ToString() }", Args(8), "8", typeof(STRING)) // FAIL - String:ToString() is overloaded!
        TestMacro(mc, e"{|a,b| a $ b}", Args("est", "test"), TRUE, typeof(boolean))
        TestMacro(mc, e"{|a,b| a $ b}", Args("test", "est"), FALSE, typeof(boolean))
        TestMacro(mc, e"{|a,b| sizeof(int) }", Args(), sizeof(INT), typeof(DWORD))
        TestMacro(mc, e"{|a,b| sizeof(teststruct) }", Args(), sizeof(teststruct), typeof(DWORD))
        TestMacro(mc, e"{|a,b| sizeof(testclass.nested.child) }", Args(), sizeof(testclass.nested.child), typeof(DWORD))
//        TestMacro(mc, e"{|a,b| testclass.nested.child.haha }", Args(), 4321, typeof(int)) // FAIL - not supported
        TestMacro(mc, e"{|a,b| testclass.nested.fff }", Args(), 333, typeof(INT))
        TestMacro(mc, e"{|a,b| default(testclass), sizeof(int) }", Args(), 4, typeof(DWORD))
        TestMacro(mc, e"{|a,b| testclass.nested.fff, sizeof(int) }", Args(), 4, typeof(DWORD))
        TestMacro(mc, e"{|a,b| 1 is int }", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| 1 is ValueType }", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| 1 is object }", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| 1 is real4 }", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| 1 is testclass }", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| 1 is teststruct }", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| 1 is testclass.nested }", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| testclass{} is testclass.nested.fff }", Args(), NULL, NULL, ErrorCode.NotAType)
        TestMacro(mc, e"{|a,b| testclass{} is testclass }", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| testclass{} is testclass.nested }", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| testclass{} is int }", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| sizeof(testclass.nested.fff) }", Args(), NULL, NULL, ErrorCode.NotAType)
        TestMacro(mc, e"{|a,b| default(testclass.nested.fff) }", Args(), NULL, NULL, ErrorCode.NotAType)
        TestMacro(mc, e"{|a,b| (testclass.nested.fff)123 }", Args(), NULL, NULL, ErrorCode.NotAType)
        TestMacro(mc, e"{|a,b| (testclass.nested)123 }", Args(), NULL, NULL, ErrorCode.NoConversion)
        TestMacro(mc, e"{|a,b| int is ValueType }", Args(), NULL, NULL, ErrorCode.NotAnExpression)
        TestMacro(mc, e"{|a,b| int }", Args(), NULL, NULL, ErrorCode.NotAnExpression)
        TestMacro(mc, e"{|a,b| U(int) }", Args(), NULL, NULL, ErrorCode.NotAnExpression)
//        TestMacro(mc, "U", Args(), null, null, ErrorCode.NotAnExpression) // It is treated as field/memvar
//        TestMacro(mc, e"{|a,b| asdgfafd(123) }", Args(), null, null, ErrorCode.NotAMethod)
        TestMacro(mc, e"{|a,b| testclass.nested(123) }", Args(), NULL, NULL, ErrorCode.NotAMethod)
        TestMacro(mc, e"{|a,b| Console.Write(null) }", Args(), NULL, typeof(OBJECT))
        TestMacro(mc, e"{|a,b| Console.Write() }", Args(), NULL, NULL, ErrorCode.NoSuitableOverload)
        TestMacro(mc, e"'AA' == U('A')", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"'AA' = U('A')", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, e"'AA' == 'A'", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"'AA' = 'A'", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, e"'AA' == (object)'A'", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"'AA' = (object)'A'", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, e"'AA' != U('A')", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"'AA' != 'A'", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"'AA' != (object)'A'", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a > b}", Args("est","test"), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a < b}", Args("est","test"), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a| a < 'a'}", Args("est"), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a| a > 'a'}", Args("est"), TRUE, typeof(LOGIC))
        TestMacro(mc, e"'A' > 'AA'", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"'A' < 'AA'", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|a| _NOT(a) }", Args(7), -8, typeof(INT))
        TestMacro(mc, e"{|a| _AND(a,a) }", Args(7), 7, typeof(INT))
        TestMacro(mc, e"{|a| _AND(a,a,a) }", Args(7), 7, typeof(INT))
        TestMacro(mc, e"{|a| _AND(a,0,a) }", Args(7), 0, typeof(INT))
        TestMacro(mc, e"_XOR(7,9)", Args(), 14, typeof(INT))
        TestMacro(mc, e"_XOR(7,7)", Args(), 0, typeof(INT))
        TestMacro(mc, e"_XOR(7,7,7)", Args(), 7, typeof(INT))
        TestMacro(mc, e"{|a| (int)a }", Args(7), 7, typeof(INT))
        TestMacro(mc, e"999999999999999999999999", Args(), NULL, NULL, ErrorCode.LiteralIntegerOverflow)
        TestMacro(mc, e"9.99999e999999999999999999", Args(), NULL, NULL, ErrorCode.LiteralFloatOverflow)
        TestMacro(mc, e"-tsi", Args(), NULL, NULL, ErrorCode.UnaryOperationNotFound)
        TestMacro(mc, e"tsi+1", Args(), NULL, NULL, ErrorCode.BinaryOperationNotFound)
        TestMacro(mc, e"tsi[2]", Args(), NULL, NULL, ErrorCode.NoConversion)
        TestMacro(mc, e"{|a,b| 1[2]}", Args(), NULL, NULL, ErrorCode.NoConversion)
        TestMacro(mc, "ArgCount(1,nil)", Args(), NULL, NULL, ErrorCode.BadNumArgs)
        TestMacro(mc, "ArgCount()", Args(), 0, typeof(INT))
        TestMacro(mc, "{|a,b|ArgCount()}", Args(), 2, typeof(INT))
        TestMacro(mc, "{|a|ArgCount()}", Args(1,2,3), 1, typeof(INT))
        TestMacro(mc, "PCount(1,nil)", Args(), NULL, NULL, ErrorCode.BadNumArgs)
        TestMacro(mc, "PCount()", Args(), 0, typeof(INT))
        TestMacro(mc, "{|a,b|PCount()}", Args(), 0, typeof(INT))
        TestMacro(mc, "{|a|PCount()}", Args(1,2,3), 3, typeof(INT))
        TestMacro(mc, e"_GetMParam(0)", Args(10, 20, 30.5), NULL, typeof(OBJECT))
        TestMacro(mc, e"_GetMParam(1)", Args(10, 20, 30.5), 10, typeof(INT))
        TestMacro(mc, e"_GetMParam(2)", Args(10, 20, 30.5), 20, typeof(INT))
        TestMacro(mc, e"_GetMParam(3)", Args(10, 20, 30.5), 30.5, typeof(REAL8))
        TestMacro(mc, e"_GetMParam(100)", Args(10, 20, 30.5), NULL, typeof(OBJECT))
        TestMacro(mc, e"_GetFParam(0)", Args(10, 20, 30.5), NULL, typeof(OBJECT))
        TestMacro(mc, e"_GetFParam(1)", Args(10, 20, 30.5), 10, typeof(INT))
        TestMacro(mc, e"_GetFParam(2)", Args(10, 20, 30.5), 20, typeof(INT))
        TestMacro(mc, e"_GetFParam(3)", Args(10, 20, 30.5), 30.5, typeof(REAL8))
        TestMacro(mc, e"_GetFParam(100)", Args(10, 20, 30.5), NULL, typeof(OBJECT))
        TestMacro(mc, e"testclass.nested.child.haha", Args(), testclass.nested.child.haha, typeof(testclass.nested.child))
        TestMacro(mc, e"testclass.nested.ttt", Args(), testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"testclass.nested.ccc", Args(), 456, typeof(INT))
        TestMacro(mc, e"testclass.nested.eee", Args(), testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"(int)testclass.nested.eee", Args(), 1, typeof(INT))
        TestMacro(mc, e"(testclass.nested.child)1", Args(), testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"(testclass.nested.child)1.5", Args(), testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"-(-testclass.nested.eee)", Args(), testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"-testclass.nested.eee", Args(), -1, typeof(testclass.nested.child))
        TestMacro(mc, e"2-testclass.nested.eee", Args(), testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"testclass.nested.eee+1", Args(), 2, typeof(testclass.nested.child))
        TestMacro(mc, e"testclass.nested.eee|testclass.nested.child.haha", Args(), testclass.nested.child.haha, typeof(testclass.nested.child))
        TestMacro(mc, e"testclass.nested.eee*2", Args(), 2, typeof(INT))
        TestMacro(mc, e"testclass.nested.eee/1", Args(), 1, typeof(INT))
        TestMacro(mc, e"1_000", Args(), 1000, typeof(INT))
        TestMacro(mc, e"1_001.1_2", Args(), 1001.12, typeof(FLOAT))
        TestMacro(mc, e"1_001.1_2e1_2", Args(), 1001.12e12, typeof(FLOAT))
        TestMacro(mc, e"1000_", Args(), NULL, NULL, ErrorCode.InvalidNumber)
        TestMacro(mc, e"1_001.1_", Args(), NULL, NULL, ErrorCode.InvalidNumber)
        TestMacro(mc, e"1_001.1_2e1_", Args(), NULL, NULL, ErrorCode.InvalidNumber)
        TestMacro(mc, e"1_001.1_e1", Args(), NULL, NULL, ErrorCode.InvalidNumber)
        TestMacro(mc, e"123.45e0m", Args(), NULL, NULL, ErrorCode.Unexpected)
        TestMacro(mc, e"--testclass.nested.eee", Args(), NULL, NULL, ErrorCode.NoAccessMode)
        TestMacro(mc, e"testclass(9)", Args(), NULL, NULL, ErrorCode.NotAMethod)
        TestMacro(mc, e"{|a| a(12332) }", Args((@@Func<INT,INT>)I), 12332, typeof(INT))
        TestMacro(mc, e"{|a|A*1_000", Args(123), 123000, typeof(INT))
        TestMacro(mc, e"{|a| USUAL(-a) }", Args(1), -1, typeof(INT))
        TestMacro(mc, e"{|a| (USUAL)(-a) }", Args(1), -1, typeof(INT))
        TestMacro(mc, e"0.00001", Args(), 1e-5, typeof(FLOAT))
        TestMacro(mc, e"{|a,b,c|DoTest(a,b,c)}", Args(1, TRUE, NIL), 5, typeof(INT))
        TestMacro(mc, e"{|a,b,c|DoTestC(a,b,c)}", Args(1, TRUE, testclass{222}), 222, typeof(INT))
        TestMacro(mc, e"{|a,b,c|DoTestS(a,b,c)}", Args(1, TRUE, teststruct{222}), 222, typeof(INT))
        TestMacro(mc, e"{|a| AScan(a, \"12\") }", Args({"135454","54376","123","53"}, NIL), 3, typeof(DWORD))
        TestMacro(mc, e"{|a| ALen(a) }", Args({"1235454","54376","12","53"},NIL), 4, typeof(DWORD))
        TestMacro(mc, e"{|a| (testclass)a }",Args(tci), tci, typeof(testclass))
        TestMacro(mc, e"{|a| ((int)a):GetHashCode() }", Args(8), 8, typeof(INT))
        TestMacro(mc, e"{|a| a:GetHashCode() }", Args(8), 8, typeof(INT))
        TestMacro(mc, e"{|a| a:GetHashCode() }", Args(tci), tci:GetHashCode(), typeof(INT))
        TestMacro(mc, e"{|a| a:GetHashCode() }", Args(tsi), tsi:GetHashCode(), typeof(INT))
        TestMacro(mc, e"{|| tci:GetHashCode() }", Args(), tci:GetHashCode(), typeof(INT))
        TestMacro(mc, e"{|| tsi:GetHashCode() }", Args(), tsi:GetHashCode(), typeof(INT))
        TestMacro(mc, e"{|a| ((int)a):ToString() }", Args(8), "8", typeof(STRING))
        TestMacro(mc, e"{|a| a:ToString() }", Args(8), "8", typeof(STRING))
        TestMacro(mc, e"{|a| a:ToString() }", Args(tci), "testclass", typeof(STRING))
        TestMacro(mc, e"{|a| a:ToString() }", Args(tsi), "teststruct", typeof(STRING))
        TestMacro(mc, e"{|| tci:ToString() }", Args(), "testclass", typeof(STRING))
        TestMacro(mc, e"{|| tsi:ToString() }", Args(), "teststruct", typeof(STRING))
        TestMacro(mc, e"{|a| a:UString() }", Args(tci), "1", typeof(STRING))
        TestMacro(mc, e"{|a| a:UString() }", Args(tsi), "1", typeof(STRING))
        TestMacro(mc, e"{|| testbase{}:FString() }", Args(), "base", typeof(STRING))
        TestMacro(mc, e"{|| ((testbase)tci):FString() }", Args(), "1", typeof(STRING))
        TestMacro(mc, e"{|| tsi:FString() }", Args(), "1", typeof(STRING))
        TestMacro(mc, e"{|| tci:FString('fff') }", Args(), "fff1", typeof(STRING))
        TestMacro(mc, e"{|| tsi:FString('fff') }", Args(), "fff1", typeof(STRING))
        TestMacro(mc, e"{|| testbase{}:NString((byte)1) }", Args(), "base", typeof(STRING))
        TestMacro(mc, e"{|| testclass{}:NString((byte)1) }", Args(), "child", typeof(STRING))
        TestMacro(mc, e"{|| ((testbase)testclass{}):NString((byte)1) }", Args(), "base", typeof(STRING))
        TestMacro(mc, e"{|| testclass{}:BString() }", Args(), "bbase", typeof(STRING))
        TestMacro(mc, "{|abc| Chr(abc) + 'B'}", Args(65), "AB", typeof(STRING))
        TestMacro(mc, "{|abc| Chr(65) + 'B'}", Args(), "AB", typeof(STRING))
        TestMacro(mc, '{|abc| Chr(65) + "BB"}', Args(), "ABB", typeof(STRING))
        TestMacro(mc, "{|abc| Chr(65):toString() + 'B'}", Args(), "AB", typeof(STRING))
        TestMacro(mc, e"{|abc| (usual)\"ABC\" + Chr(123)}", Args(), "ABC"+Chr(123), typeof(STRING)) 
        TestMacro(mc, e"0x1234", Args(), 0x1234, typeof(INT))
        TestMacro(mc, e"0b110011", Args(), 0b110011, typeof(INT))
        TestMacro(mc, e"0xFFFF", Args(), 0xFFFF, typeof(INT))
        TestMacro(mc, e"0XFFFF", Args(), 0xFFFF, typeof(INT))
        TestMacro(mc, e"0xffff", Args(), 0xFFFF, typeof(INT))
        TestMacro(mc, "1+1 ", Args(), 2, typeof(INT))
        TestMacro(mc, e"{|a|Len(a) == 4}", Args("test"), TRUE, typeof(LOGIC))
        TestMacro(mc, e"'a' $ 'b'", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"'a' $ 'a'", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, e"iif('a' $ 'a' , TRUE, FALSE)", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, e"iif('a' $ 'a' , 1, 2)", Args(), 1, typeof(INT))
        TestMacro(mc, e"right('abcdef',3)", Args(), "def", typeof(STRING))
        TestMacro(mc, e"left('abcdef',3)", Args(), "abc", typeof(STRING))
        TestMacro(mc, e"left('abcdef',3+1)", Args(), "abcd", typeof(STRING))
        TestMacro(mc, e"{|o|o:fld := ctest{1,0}}", Args(ctest{0,0}), ctest{1,0}, typeof(ctest))
        TestMacro(mc, e"ctest{ctest{1,2}}", Args(), ctest{0,0}, typeof(ctest))
        TestMacro(mc, e"Foo{Foo{}}", Args(), Foo{}, typeof(Foo))
        TestMacro(mc, e"{|o|l := true, l := l .or. (o:fieldget(#F1)!= o:propget(#F2, '-') .or. o:fieldget(#F1)!= o:propget(#F3, '-') .or. o:fieldget(#F1)!= o:propget(#F4, '-'))}", Args(ctest{1,2}), TRUE, typeof(LOGIC))
        TestMacro(mc, "foo", Args(), NULL, NULL)
        TestMacro(mc, "foo := 1234, foo", Args(), 1234, typeof(INT))
        TestMacro(mc, "ToString()", Args(), null, null, ErrorCode.NoStaticOverload)
        TestMacro(mc, e"{|| !\"T\"$\"Test\"} ", Args(), false, typeof(logic))
        TestMacro(mc, e"testDef( \"MDOKU\" )", Args(), "testDef", typeof(string))
        TestMacro(mc, e"testDef( \"MDOKU\",,,,,,,,NULL )", Args(), "testDef", typeof(string))
        TestMacro(mc, 'e"abc" + e"def"', Args(), e"abc" + e"def", typeof(string))
        TestMacro(mc, 'e"a\\bc" + e"def"', Args(), e"a\\bc" + e"def", typeof(string))
        TestMacro(mc, 'e"a\"bc" + e"def"', Args(), e"a\"bc" + e"def", typeof(string))
        TestMacro(mc, 'e"a\"bc" + e"d\\ef"', Args(), e"a\"bc" + e"d\\ef", typeof(string))
        TestMacro(mc, "SubStr3('Test', 1 , SLen('abc') + 1)", Args(), "Test", typeof(string)) // should raise warning
        TestMacro(mc, "SubStr3('Test', 1 , Len('abc') - 1)", Args(), "Te", typeof(string)) // should raise warning
        TestMacro(mc, "TestInt32(TestDWord(1))", Args(), 1, typeof(int)) // should raise warning
        TestMacro(mc, "TestDWord(TestInt32(1))", Args(), 1, typeof(dword)) // should raise warning
        TestMacro(mc, e"{|a| tci:&a := 1 }", Args("v1"), 1, typeof(INT))
        TestMacro(mc, e"{|a| tci:&a }", Args("v1"), 1, typeof(INT))
        TestMacro(mc, e"{|a| tci:&(a) }", Args("v1"), 1, typeof(INT))
        TestMacro(mc, e"{|a| tci:(&a) }", Args("v1"), 1, typeof(INT))
        TestMacro(mc, e"{|a,b| a := \"abcdef\", a:(&b)() }", Args(8,"ToUpperInvariant"), "ABCDEF", typeof(STRING))
        TestMacro(mc, e"{|a,b| a := \"abcdef\", (a:&b)() }", Args(8,"ToUpperInvariant"), "ABCDEF", typeof(STRING))
        TestMacro(mc, e"{|a,b| a := \"abcdef\", a:&(b)() }", Args(8,"ToUpperInvariant"), NULL, NULL, ErrorCode.Expected)
        TestMacro(mc, e"{|a,b| a := \"abcdef\", (a:&testRet(b))() }", Args(8,"ToUpperInvariant"), "ABCDEF", typeof(STRING))
        TestMacro(mc, e"{|a,b| a := \"abcdef\", a:&testRet(b)() }", Args(8,"ToUpperInvariant"), NULL, NULL, ErrorCode.ArgumentsNotMatch)
        TestMacro(mc, e"{|a,b| a := \"abcdef\", a:(&b[1])() }", Args(8,{"ToUpperInvariant"}), "ABCDEF", typeof(STRING))
        TestMacro(mc, e"{|a,b| a := \"abcdef\", (a:&b[1])() }", Args(8,{"ToUpperInvariant"}), "ABCDEF", typeof(STRING))
        TestMacro(mc, e"{|| Error}", Args(), 321, typeof(int))
        TestMacro(mc, e"{|| Error{\"TEST\"}:Message}", Args(), "TEST", typeof(string))
        TestMacro(mc, e"{|| Error := 123}", Args(), 123, typeof(int))
        TestMacro(mc, e"{|| ErrorLevel}", Args(), 1, typeof(int))
        TestMacro(mc, e"{|| ErrorLevel()}", Args(), 0, typeof(dword))
        TestMacro(mc, e"{|| ErrString(0)}", Args(), "", typeof(string))
        TestMacro(mc, e"{|| ErrString.V}", Args(), 333, typeof(int))

        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsField
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGet, "MyFieldGet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSet, "MyFieldSet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGetWa, "MyFieldGetWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSetWa, "MyFieldSetWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___pushWorkarea, "MyPushWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___popWorkarea, "MyPopWa")
        TestMacro(mc, "U", Args(), "FieldGet(U)", typeof(STRING))
        TestMacro(mc, e"{|| NIKOS}", Args(), "FieldGet(NIKOS)", typeof(STRING))
        TestMacro(mc, e"{|| NIKOS := \"123\"}", Args(), "FieldSet(NIKOS):123", typeof(STRING))
        TestMacro(mc, e"{|| _FIELD->NIKOS}", Args(), "FieldGet(NIKOS)", typeof(STRING))
        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS}", Args(), "FieldGet(BASE,NIKOS)", typeof(STRING))
        TestMacro(mc, e"{|| FIELD->NIKOS}", Args(), "FieldGet(NIKOS)", typeof(STRING))
        TestMacro(mc, e"{|| FIELD->BASE->NIKOS}", Args(), "FieldGet(BASE,NIKOS)", typeof(STRING))
        TestMacro(mc, e"{|| BASE->NIKOS}", Args(), "FieldGet(BASE,NIKOS)", typeof(STRING))
        TestMacro(mc, e"{|| _FIELD->NIKOS := \"123\"}", Args(), "FieldSet(NIKOS):123", typeof(STRING))
        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS := \"123\"}", Args(), "FieldSet(BASE,NIKOS):123", typeof(STRING))
        TestMacro(mc, e"{|| FIELD->NIKOS := \"123\"}", Args(), "FieldSet(NIKOS):123", typeof(STRING))
        TestMacro(mc, e"{|| FIELD->BASE->NIKOS := \"123\"}", Args(), "FieldSet(BASE,NIKOS):123", typeof(STRING))
        TestMacro(mc, e"{|| BASE->NIKOS := \"123\"}", Args(), "FieldSet(BASE,NIKOS):123", typeof(STRING))
        TestMacro(mc, e"{|| (\"BASE\")->NIKOS}", Args(), "BASE->FieldGet(NIKOS)", typeof(STRING))
        TestMacro(mc, e"{|| (\"BASE\")->NIKOS := \"123\"}", Args(), "BASE->FieldSet(NIKOS):123", typeof(STRING))
        TestMacro(mc, e"{|a,b| (a)->&b}", Args("DEVELOPER","ROBERT"), "DEVELOPER->FieldGet(ROBERT)", typeof(STRING))
        TestMacro(mc, e"{|a,b| (a)->&(b+'')}", Args("DEVELOPER","ROBERT"), "DEVELOPER->FieldGet(ROBERT)", typeof(STRING))
        TestMacro(mc, e"{|a,b| (a)->&b := 321}", Args("DEVELOPER","ROBERT"), "DEVELOPER->FieldSet(ROBERT):321", typeof(STRING))
        TestMacro(mc, e"{|a,b| (a)->&b[1]}", Args("DEVELOPER",{"ROBERT"}), "DEVELOPER->FieldGet(ROBERT)", typeof(STRING))
        TestMacro(mc, e"{|a,b| (a)->&b[1] := 123}", Args("DEVELOPER",{"ROBERT"}), "DEVELOPER->FieldSet(ROBERT):123", typeof(STRING))
        TestMacro(mc, e"{|a,b| (a)->&(b[1])}", Args("DEVELOPER",{"ROBERT"}), "DEVELOPER->FieldGet(ROBERT)", typeof(STRING))
        TestMacro(mc, e"{|a,b| (a)->&(b[1]) := 123}", Args("DEVELOPER",{"ROBERT"}), "DEVELOPER->FieldSet(ROBERT):123", typeof(STRING))
        TestMacro(mc, e"{|a,b,c| (a)->&(b+c)}", Args("DEVELOPER","ROB","ERT"), "DEVELOPER->FieldGet(ROBERT)", typeof(STRING))
        TestMacro(mc, e"{|a,b,c| (a)->&(b+c) := 321}", Args("DEVELOPER","ROB","ERT"), "DEVELOPER->FieldSet(ROBERT):321", typeof(STRING))
        TestMacro(mc, e"{|a,b| (a)->(MyDbDo(b))}", Args("BASE","ARG"), "BASE->Do(ARG)", typeof(STRING))
        ParseMacro(mc, e"{|a,b| BAse->(MyDbDo(b))}")
        TestMacro(mc, e"{|a,b| BAse->(MyDbDo(b))}", Args("BASE","ARG"), "BAse->Do(ARG)", typeof(STRING))
        TestMacro(mc, e"{|a,b| BASe->MyDbDo(b)}", Args("BASE","ARG"), "BASe->Do(ARG)", typeof(STRING))
        TestMacro(mc, e"{|a,b| MyDbDo(b)}", Args("BASE","ARG"), "Do(ARG)", typeof(STRING))

        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsFieldOrMemvar
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___VarGet, "MyVarGet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___VarPut, "MyVarPut")
        TestMacro(mc, e"{|| NIKOS}", Args(), "VarGet(NIKOS)", typeof(STRING))
        TestMacro(mc, e"{|| NIKOS := \"123\"}", Args(), "VarPut(NIKOS):123", typeof(STRING))
        TestMacro(mc, e"{|a,b| asdgfafd(123) }", Args(), NULL, NULL, ErrorCode.NotAMethod)
        TestMacro(mc, e"{|| CODE+SET}", Args(), "VarGet(CODE)VarGet(SET)", typeof(STRING))
        TestMacro(mc, e"{|| LONG}", Args(), "VarGet(LONG)", typeof(STRING))

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

        VAR mc := XSharp.Runtime.MacroCompiler{}

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

