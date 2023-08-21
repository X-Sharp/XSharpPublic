// TestData.prg
// Created by    : nvk
// Creation Date : 2/13/2021 4:08:09 PM
// Created for   :
// WorkStation   : I7
#pragma warnings (660, off)
#pragma warnings (661, off)
#pragma warnings (108, off)


USING System
USING System.Collections.Generic
USING System.Text

FUNCTION FoxArrayTest(a1, a2, a3) CLIPPER
    local result := 42 as INT
    FOREACH var arg in _Args()
        IF IsLong(arg)
            result += arg
        ENDIF
    next
    RETURN result


FUNCTION TestI(i as int) as int
    return i

FUNCTION Args(args PARAMS OBJECT[]) AS OBJECT[]
    RETURN args

FUNCTION ArgsU(args PARAMS USUAL[]) AS USUAL[]
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

FUNCTION IncrInt(i REF INT) AS INT
    i := i+1
    RETURN i

FUNCTION AllTrim(cValue as STRING) AS STRING
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

    CLASS CheckBox
        PROPERTY Checked AS LOGIC AUTO
    END CLASS
    CLASS TestWithItem2
        PROPERTY Item as LONG  GET 42
        PROPERTY Nested AS USUAL GET TestWithItem2{}
    END CLASS


GLOBAL tsi := teststruct{1} AS teststruct
GLOBAL tci := testclass{1} AS testclass

global wag := "" as string

    public CLASS TestGlobals
    static public tsi := teststruct{1} AS teststruct
    static public tci := testclass{1} AS testclass
    static public tsa := <int>{11, 22, 33, 44, 55} AS INT[]
    static public tsa2 := int[,]{2,2} {{11,22},{33,44}} AS INT[,]
        static public wag := "" as string
    END CLASS

    FUNCTION MyVarGet(name AS STRING) AS USUAL
        RETURN wag + "VarGet(" + name + ")"

    FUNCTION MyVarPut(name AS STRING, VALUE AS USUAL) AS USUAL
        RETURN wag + "VarPut(" + name +"):" + VALUE:ToString()

    FUNCTION MyFoxVarPut(name AS STRING, VALUE AS USUAL) AS USUAL
        RETURN wag + "FoxVarPut(" + name +"):" + VALUE:ToString()

    FUNCTION MyMemVarGet(name AS STRING) AS USUAL
        RETURN "MemVarGet(" + name + ")"

    FUNCTION MyMemVarPut(name AS STRING, VALUE AS USUAL) AS USUAL
        RETURN "MemVarPut(" + name +"):" + VALUE:ToString()

    FUNCTION MyFoxMemVarPut(name AS STRING, VALUE AS USUAL) AS USUAL
        RETURN "FoxMemVarPut(" + name +"):" + VALUE:ToString()

    FUNCTION MyFieldGet(name AS STRING) AS USUAL
        RETURN wag + "FieldGet(" + name + ")"

    FUNCTION MyFieldSet(name AS STRING, VALUE AS USUAL) AS USUAL
        RETURN wag + "FieldSet(" + name +"):" + VALUE:ToString()



    FUNCTION MyFieldGetWa(wa AS STRING, name AS STRING) AS USUAL
        RETURN "FieldGet(" + wa + "," + name + ")"

    FUNCTION MyFieldSetWa(wa AS STRING, name AS STRING, VALUE AS USUAL) AS USUAL
        RETURN "FieldSet(" + wa + "," + name +"):" + VALUE:ToString()

    FUNCTION MyFoxFieldSetWa(wa AS STRING, name AS STRING, VALUE AS USUAL) AS USUAL
        RETURN "FoxFieldSet(" + wa + "," + name +"):" + VALUE:ToString()

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

    begin namespace TestNS.Nested
        class TestClass
            end class
    end namespace

    CLASS TestNS2.TestClass
    CONSTRUCTOR()
        STATIC METHOD TestMethod() AS INT STRICT
            RETURN 123
        END CLASS

    FUNCTION TestByRef(s REF STRING) AS STRING
        RETURN s

GLOBAL TEST := 123
DEFINE TEST2 := 123

FUNCTION testfunc(y REF STRING) AS VOID
    y := "b"

FUNCTION testfunclate(y) AS VOID
    y := "b"

CLASS TestRefCall

METHOD testmethod(y REF STRING) AS VOID
    y := "b"

METHOD testmethodlate(y) AS VOID
    y := "b"


END CLASS


FUNCTION S_EnforceType(uVar REF USUAL,cTyp AS STRING) AS VOID
    IF ValType(uVar) <> cTyp
        SWITCH cTyp
            CASE "C"
                uVar := ""
            CASE "N"
                uVar := 0
            CASE "D"
                uVar := NULL_DATE
            CASE "L"
                uVar := FALSE
            CASE "A"
                uVar := {}
            CASE "O"
                uVar := NULL_OBJECT
            CASE "#"
                uVar := NULL_SYMBOL
            CASE "U"
                uVar := NIL
        END SWITCH
    ENDIF
    RETURN

FUNCTION S_EnforceTypeC(uVar,cTyp) CLIPPER
    IF ValType(uVar) <> cTyp
        SWITCH cTyp
            CASE "C"
                uVar := ""
            CASE "N"
                uVar := 0
            CASE "D"
                uVar := NULL_DATE
            CASE "L"
                uVar := FALSE
            CASE "A"
                uVar := {}
            CASE "O"
                uVar := NULL_OBJECT
            CASE "#"
                uVar := NULL_SYMBOL
            CASE "U"
                uVar := NIL
        END SWITCH
    ENDIF
    RETURN NIL

// bug 1186
BEGIN NAMESPACE MyNS
	CLASS StaticClass
		EXPORT Test := 12345 AS INT
	END CLASS
END NAMESPACE


