using System
using System.Collections.Generic
using System.Linq
using System.Text
using XSharp.Runtime
using XSharp.MacroCompiler

function Args(args params object[]) as object[]
    return args

function U(u as usual) as usual
    return u

function U(s as string) as usual
    return s

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
    public v1 as int
    public v2 as string

    override method GetHashCode() as int
        return super:GetHashCode()
    override method Equals(o as object) as logic
        return self == o
    operator ==(o1 as testclassdc, o2 as testclassdc) as logic
        return o1:v1 == o2:v1 .and. o1:v2 == o2:v2
    operator !=(o1 as testclassdc, o2 as testclassdc) as logic
        return !(o1 == o2)
end class

class testbase
    method BString() as string
        return "bbase"

    method nString(x as long) as string
        return "base"

    virtual method FString() as string
        return "base"
end class

class testclass inherit testbase
    class nested
        enum child
            haha := 4321
            blabla := 1
        end enum
        enum child2
            haha := 432
            blabla := 12
        end enum

        public static ttt := child.blabla as child

        public static fff := 333 as int

        public const ccc := 456 as int

        public const eee := child.blabla as child
    end class

    public v1 as int
    public v2 as string

    static property sprop as int auto get set
    property prop as int auto get set

    constructor()
    constructor(i as int)
        v1 := i
        prop := i

    method UString() as string
        return v1:ToString()

    method NString(x as long) as string
        return "child"

    override method FString() as string
        return v1:ToString()
    method FString(prefix as string) as string
        return prefix+v1:ToString()

    override method GetHashCode() as int
        return super:GetHashCode()
    override method Equals(o as object) as logic
        return self == o
    operator ==(o1 as testclass, o2 as testclass) as logic
        return o1:v1 == o2:v1 .and. o1:v2 == o2:v2
    operator !=(o1 as testclass, o2 as testclass) as logic
        return !(o1 == o2)
end class

struct teststruct
    public v1 as int
    public v2 as string

    static property sprop as int auto get set
    property prop as int auto get set

    constructor(i as int)
        v1 := i
        v2 := null
        prop := i

    method UString() as string
        return v1:ToString()

    method FString() as string
        return v1:ToString()
    method FString(prefix as string) as string
        return prefix+v1:ToString()

    override method GetHashCode() as int
        return super:GetHashCode()
    override method Equals(o as object) as logic
        return self == (teststruct)o
    operator ==(o1 as teststruct, o2 as teststruct) as logic
        return o1:v1 == o2:v1 .and. o1:v2 == o2:v2
    operator !=(o1 as teststruct, o2 as teststruct) as logic
        return !(o1 == o2)
end struct

global tsi := teststruct{1} as teststruct

global tci := testclass{1} as testclass

function MyVarGet(name as string) as usual
    return "VarGet(" + name + ")"

function MyVarPut(name as string, value as usual) as usual
    return "VarPut(" + name +"):" + (string)value

function MyFieldGet(name as string) as usual
    return "FieldGet(" + name + ")"

function MyFieldSet(name as string, value as usual) as usual
    return "FieldSet(" + name +"):" + (string)value

function MyFieldGetWa(wa as string, name as string) as usual
    return "FieldGet(" + wa + "," + name + ")"

function MyFieldSetWa(wa as string, name as string, value as usual) as usual
    return "FieldSet(" + wa + "," + name +"):" + (string)value

function DoTest(n as int, l as logic, o as System.Collections.ArrayList) as int
    return n * 5

function DoTestC(n as int, l as logic, o as testclass) as int
    return o:v1

function DoTestS(n as int, l as logic, o as teststruct) as int
    return o:v1

class ctest
    public property fld as ctest auto
    public a as real8
    public b as real8
    constructor (a_ as real8, b_ as real8)
        a := a_
        b := b_
    operator ==(o1 as ctest, o2 as ctest) as logic
        return o1:a == o2:a .and. o1:b == o2:b
    operator !=(o1 as ctest, o2 as ctest) as logic
        return !(o1 == o2)
end class

begin namespace MacroCompilerTest

	function Start() as void
	    SetMacroCompiler(typeof(XSharp.Runtime.MacroCompiler))

        ReportMemory("initial")
        var mc := CreateMacroCompiler()

        //ParseMacro(mc, e"{|a,b| +a[++b] += 100, a[2]}")
        //EvalMacro(mc, e"{|a,b| a[++b] += 100, a[2]}", {1,2,3}, 1)
        //EvalMacro(mc, e"{|a|A,1_000", 123)
        //EvalMacro(mc, e"{|a| USUAL(-a) }", 1)
        //EvalMacro(mc, e"{|| testclass{}:NString((byte)1) }", Args())
        //EvalMacro(mc, e"{|a,b| b := testclass{123}, b:ToString() }")
        EvalMacro(mc, e"0.00001")
        //EvalMacro(mc, "{|foo| bar := 10}")
        //EvalMacro(mc, "{|foo| bar := 10,foo}")
//        EvalMacro(mc, e"oWindow:Background:= Brush{Color{ 200, 200, 220},0}}")
//        EvalMacro(mc, e"l := true, l := l .or. oserver:fieldget(#FUERMITARB)!= owindow:getproperty(#oldFUERMITARB, '-')")
//        EvalMacro(mc, e"l := oserver:fieldget(#FUERMITARB)!= owindow:getproperty(#oldFUERMITARB, '-')")
        wait

        RunTests(mc)
        wait

        RunPerf(mc, "Console.WriteLine(123)")

        ReportMemory("final");

        Console.WriteLine("Press any key to exit...")
        Console.ReadKey()

    function ParseMacro(mc as XSharp.Runtime.MacroCompiler, src as string) as void
        Console.WriteLine("Parsing macro ...")
        var ast := mc:compiler:Parse(src)
        Console.WriteLine(ast)

    function EvalMacro(mc as XSharp.Runtime.MacroCompiler, src as string, args params object[]) as usual
        Console.WriteLine("Executing macro ...")
        try
            //var cb := MCompile(src)
            var cb := mc:Compile(src)
            var res := cb:EvalBlock(args)
            Console.WriteLine("res = {0}",res)
            return res
        catch e as CompilationError
            Console.WriteLine("{0}",e:Message)
            return nil
        end

    global TotalFails := 0 as int
    global TotalTests := 0 as int
    global TotalSuccess := 0 as int

    function RunTests(mc as XSharp.Runtime.MacroCompiler) as void
        Console.WriteLine("Running tests ...")

        TestParse(mc, e"{|a,b| +a[++b] += 100, a[2]}", "{|a, b|((+a((++b)))+='100'), a('2')}")
        TestMacro(mc, e"{|a,b| asdgfafd(123) }", Args(), null, null,ErrorCode.NotAMethod)

        mc:Options:UndeclaredVariableResolution := VariableResolution.Error
        TestMacro(mc, e"{|a,b| testtest__() }", Args(1,2,3), null, null, ErrorCode.IdentifierNotFound)

        mc:Options:UndeclaredVariableResolution := VariableResolution.GenerateLocal
        TestMacro(mc, e"{|a| a() }", Args((@@Func<int>){ => 1234}), 1234, typeof(int))
        TestMacro(mc, "#HELLo", Args(), #hello, typeof(symbol))
        TestMacro(mc, "#HELLo + #World", Args(), #hello + #world, typeof(string))
        TestMacro(mc, e"#HELLo + \"world\"", Args(), #hello + "world", typeof(string))
        TestMacro(mc, e"\"Hello\" + #world", Args(), "Hello" + #world, typeof(string))
        TestMacro(mc, "U(12345)", Args(), 12345, typeof(int))
        TestMacro(mc, "U(U(12345)-1)", Args(), 12344, typeof(int))
        TestMacro(mc, "I(123+45)", Args(), 123+45, typeof(int))
        TestMacro(mc, "R(123)", Args(), 123, typeof(real8))
        TestMacro(mc, "R(123.456)", Args(), 123.456, typeof(real8))
        TestMacro(mc, "U(123.456)", Args(), 123.456, typeof(float))
        TestMacro(mc, "123.456", Args(), 123.456, typeof(float))
        TestMacro(mc, "123.456s", Args(), 123.456s, typeof(real4))
        TestMacro(mc, "123.456d", Args(), 123.456d, typeof(real8))
        TestMacro(mc, "123.450m", Args(), 123.450m, typeof(decimal))
        TestMacro(mc, "123.450m+321", Args(), 444.450m, typeof(decimal))
        TestMacro(mc, "123.450m+321.05", Args(), 444.500m, typeof(decimal))
        TestMacro(mc, "{|a,b,c|a := b := 1343+1}", Args(), 1343+1, typeof(int))
        TestMacro(mc, "{|a,b,c|}", Args(), null, typeof(object))
        TestMacro(mc, "{|a,b,c|1234}", Args(), 1234, typeof(int))
        TestMacro(mc, "1234", Args(), 1234, typeof(int))
        TestMacro(mc, "12 == 12", Args(), true, typeof(logic))
        TestMacro(mc, "", Args(), null, typeof(object))
        TestMacro(mc, "2018.12.31", Args(), 2018.12.31, typeof(date))
        TestMacro(mc, "2018.1.1", Args(), 2018.1.1, typeof(date))
        TestMacro(mc, "2018.12.31 == 2018.12.31", Args(), true, typeof(logic))
        TestMacro(mc, "2018.12.31 = 2018.12.31", Args(), true, typeof(logic))
        TestMacro(mc, "2018.12.31 = 2018.1.1", Args(), false, typeof(logic))
        TestMacro(mc, "2018.12.31 != 2018.12.31", Args(), false, typeof(logic))
        TestMacro(mc, "2018.12.31 != 2018.1.1", Args(), true, typeof(logic))
        TestMacro(mc, "null", Args(), null, typeof(object))
        TestMacro(mc, "null_object", Args(), null_object, null)
        TestMacro(mc, "null_string", Args(), null_string, null)
        TestMacro(mc, "null_psz = psz._NULL_PSZ", Args(), true, typeof(logic))
        TestMacro(mc, "null_symbol", Args(), null_symbol, typeof(symbol))
        TestMacro(mc, "null_date", Args(), null_date, typeof(date))
        TestMacro(mc, "null_codeblock", Args(), null_codeblock, null)
        TestMacro(mc, "null_ptr", Args(), null_ptr, null)
        TestMacro(mc, "{|a,b,c|a := b := 1343, c := a + 1, a+b-c/2}", Args(), 1343+1343-(1343+1)/2, typeof(int))
        TestMacro(mc, "{|a|a := 1343, a += 1}", Args(), 1343+1, typeof(int))
        TestMacro(mc, "{|a|a := -1343, a := -a}", Args(), 1343, typeof(int))
        TestMacro(mc, "{|a|a := 8, ++a, ++a}", Args(123), 10, typeof(int))
        TestMacro(mc, "{|a|a := 8, ++a, a++, a++}", Args(123), 10, typeof(int))
        TestMacro(mc, "{|a|++a, a++, a++}", Args(8), 10, typeof(int))
        TestMacro(mc, "{|a| a++, U(a++), a++}", Args(8), 10, typeof(int))
        TestMacro(mc, e"{|a| a := \"abc\" + \"def\"}", Args(8), "abcdef", typeof(string))
        TestMacro(mc, e"{|a| \"abc\" == \"def\"}", Args(8), false, typeof(logic))
        TestMacro(mc, e"{|a| \"abc\" = \"abc\"}", Args(8), true, typeof(logic))
        TestMacro(mc, e"{|a| \"abc\" != \"abc\"}", Args(8), false, typeof(logic))
        TestMacro(mc, e"{|a| a := \"abc\", a == \"abc\"}", Args(8), true, typeof(logic))
        TestMacro(mc, e"{|a| a := \"abc\", a + \"def\"}", Args(8), "abcdef", typeof(string))
        TestMacro(mc, e"{|a| 0 == 0}", Args(8), true, typeof(logic))
        TestMacro(mc, e"{|a| 0 != 0}", Args(8), false, typeof(logic))
        TestMacro(mc, e"{|a| (0 > 1) .and. (0 < 1) }", Args(8), false, typeof(logic))
        TestMacro(mc, e"{|a| a := \"qwerty\", a:Length }", Args(8), 6, typeof(int))
        TestMacro(mc, e"{|a| a := default(int) }", Args(8), 0, typeof(int))
        TestMacro(mc, e"{|a| a := default(string) }", Args(8), null, typeof(object))
        TestMacro(mc, e"{|a| a := default(usual) }", Args(8), null, typeof(object))
        TestMacro(mc, e"{|a| a := U(1234+1), a }", Args(8), 1234+1, typeof(int))
        TestMacro(mc, e"{|a| UU := U(1234+1), UU }", Args(8), 1234+1, typeof(int))
        TestMacro(mc, e"{|a| a := \"abcdef\", a:ToUpperInvariant() }", Args(8), "ABCDEF", typeof(string))
        TestMacro(mc, e"{|a| a := NIL }", Args(8), null, typeof(object))
        TestMacro(mc, e"{|| I3(4,4,4) }", Args(), 12, typeof(int))
        TestMacro(mc, e"{|a| a := I3(4,4,) }", Args(), 11, typeof(int))
        TestMacro(mc, e"{|a| a := I3(4,,4) }", Args(), 10, typeof(int))
        TestMacro(mc, e"{|a| a := I3(,4,4) }", Args(), 9, typeof(int))
        TestMacro(mc, e"{|a| a := I3(,,) }", Args(), 6, typeof(int))
        TestMacro(mc, e"{|a| a := I3(4,4) }", Args(), 11, typeof(int))
        TestMacro(mc, e"{|a| a := I3(4) }", Args(), 9, typeof(int))
        TestMacro(mc, e"{|a| a := I3() }", Args(), 6, typeof(int))
        TestMacro(mc, e"{|a| a := I0() }", Args(), 123, typeof(int))
        TestMacro(mc, e"{|a| a := CC(1,2,3) }", Args(), 6, typeof(int))
        TestMacro(mc, e"{|a| a := CC(1,2,3,4) }", Args(), 6, typeof(int))
        TestMacro(mc, e"{|a| a := CC() }", Args(), 0, typeof(int))
        TestMacro(mc, e"{|a| a := CC(1,2) }", Args(), 3, typeof(int))
        TestMacro(mc, e"{|a| a := CC(,1,2) }", Args(), 3, typeof(int))
        TestMacro(mc, e"{|a| a := CC(1,,2) }", Args(), 3, typeof(int))
        TestMacro(mc, e"{|a| a := U({1,2,3}) }", Args(), {1,2,3}, typeof(array))
        TestMacro(mc, e"{|a| a := U({1,,2,,}) }", Args(), {1,NIL,2,NIL,NIL}, typeof(array))
        TestMacro(mc, e"{|a| a := <INT>{1,2,3} }", Args(), <INT>{1,2,3}, typeof(int[]))
        TestMacro(mc, e"object{}", Args(), object{}, typeof(object))
        TestMacro(mc, e"teststruct{12}", Args(), teststruct{12}, typeof(teststruct))
        TestMacro(mc, e"teststruct{}", Args(), teststruct{}, typeof(teststruct))
        TestMacro(mc, e"testclass{}", Args(), testclass{}, typeof(testclass))
        TestMacro(mc, e"testclass{23}", Args(), testclass{23}, typeof(testclass))
        TestMacro(mc, e"testclassdc{}", Args(), testclassdc{}, typeof(testclassdc))
        TestMacro(mc, e"int{}", Args(), 0, typeof(int))
        TestMacro(mc, e"{|z| A(z) }", Args(123), 123, typeof(int32))
        TestMacro(mc, e"{|z| A(z), z }", Args(123), 1123, typeof(int32))
        TestMacro(mc, e"{|a| testclass.sprop := 555, a := ++testclass.sprop }", Args(), 556, typeof(int32))
        TestMacro(mc, e"{|a| testclass.sprop := a, a := ++testclass.sprop }", Args(55), 56, typeof(int32))
        TestMacro(mc, e"{|a| teststruct.sprop := 555, a := ++teststruct.sprop }", Args(), 556, typeof(int32))
        TestMacro(mc, e"{|a| teststruct.sprop := a, a := ++teststruct.sprop }", Args(55), 56, typeof(int32))
        TestMacro(mc, e"{|a| a := testclass{}, a:prop }", Args(), 0, typeof(int))
        TestMacro(mc, e"{|a| a := testclass{222}, a:prop }", Args(), 222, typeof(int))
        TestMacro(mc, e"{|a| a := teststruct{}, a:prop }", Args(), 0, typeof(int))
        TestMacro(mc, e"{|a| a := teststruct{222}, a:prop }", Args(), 222, typeof(int))
        TestMacro(mc, e"{|a| a := testclass{}, a:v1 := 1 }", Args(), 1, typeof(int))
        TestMacro(mc, e"{|a| a := testclass{222}, a:v1 }", Args(), 222, typeof(int))
        TestMacro(mc, e"{|a| a := teststruct{}, a:v1 := 1 }", Args(), 1, typeof(int))
        TestMacro(mc, e"{|a| a := teststruct{222}, a:v1 }", Args(), 222, typeof(int))
        TestMacro(mc, e"{|a| a := testclass{}, a:prop := 111 }", Args(), 111, typeof(int))
        TestMacro(mc, e"{|a,b| b := testclass{}, b:prop := a, ++b:prop }", Args(55), 56, typeof(int))
        TestMacro(mc, e"{|| tsi:v1 }", Args(), 1, typeof(int))
        TestMacro(mc, e"{|| ++tsi:v1 }", Args(), 2, typeof(int))
//        TestMacro(mc, e"{|| tsi:v1 := 10, tsi:v1 }", Args(), 10, typeof(int)) // FAIL because tsi is boxed by value for IVarPut()
        TestMacro(mc, e"{|| ++tsi:prop }", Args(), 2, typeof(int))
        TestMacro(mc, e"{|| tci:v1 }", Args(), 1, typeof(int))
        TestMacro(mc, e"{|| ++tci:v1 }", Args(), 2, typeof(int))
        TestMacro(mc, e"{|| ++tci:prop }", Args(), 2, typeof(int))
        TestMacro(mc, e"{|| tci:v1 := 10, tci:v1++, tci:v1 }", Args(), 11, typeof(int))
        TestMacro(mc, e"{|a| IIF(a>10,123,1.23) }", Args(100), 123, typeof(float))
        TestMacro(mc, e"{|a| IIF(a>10,123,1.23) }", Args(1), 1.23, typeof(float))
        TestMacro(mc, e"{|a| IIF(a>10,1) }", Args(100), 1, typeof(int))
        TestMacro(mc, e"{|a| IIF(a>10,,1) }", Args(100), null, typeof(object))
        TestMacro(mc, e"{|a| (float)++a/2 }", Args(2), 1.5, typeof(float))
        TestMacro(mc, e"{|a| a := {1,2,3,4}, a[1] := 10, a[1] + a[2] }", Args(), 12, typeof(int))
        TestMacro(mc, e"{|a| a := {1,2,3,4}, a[1] += 10, a[1] }", Args(), 11, typeof(int))
        TestMacro(mc, "{|a|a := 8, a := 8**a}", Args(123), 16777216, typeof(float))
        TestMacro(mc, "I((int)123.456)", Args(), 123, typeof(int))
        TestMacro(mc, "{|a| b := 8, c := b**a, c}", Args(8), 16777216, typeof(float))
        TestMacro(mc, "{|a,b,c|a.and.b.or..not.c}", Args(true,false,true), false, typeof(logic))
        TestMacro(mc, "{|a| a := U({1,2,3", Args(), {1,2,3}, typeof(array))
//        TestMacro(mc, e"{|| _FIELD->NIKOS}", Args(), nil, typeof(object))
//        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS}", Args(), nil, typeof(object))
//        TestMacro(mc, e"{|| BASE->NIKOS}", Args(), nil, typeof(object))
//        TestMacro(mc, e"{|| _FIELD->NIKOS := 123}", Args(), 123, typeof(object))
//        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS := 123}", Args(), 123, typeof(object))
//        TestMacro(mc, e"{|| BASE->NIKOS := 123}", Args(), 123, typeof(object))
        TestMacro(mc, e"{|a,b| a[++b] += 100, a[2]}", Args({1,2,3}, 1), 102, typeof(int))
        TestMacro(mc, e"_chr(65)", Args(), Chr(65), typeof(string))
        TestMacro(mc, e"chr(65)", Args(), Chr(65), typeof(string))
        TestMacro(mc, e"char(65)", Args(), 65, typeof(char))
        TestMacro(mc, e"slen(\"hello\")", Args(), 5, typeof(dword))
        TestMacro(mc, e"{|v| v[2] }", Args( <object>{ { 'C', 100, 0} } ),100, typeof(int))
        TestMacro(mc, e"{|v| v[2,1,2,1,1] }", <object>{ {{}, {{ "1_78", {{ 'DATEI_1', 'C', 100, 0,'Anhang 1','Anhang1' }}, nil, nil }}} },"DATEI_1", typeof(string))
//        TestMacro(mc, e"{|v| v[2,1,2,1,1] := 'TEST', v[2,1,2,1,1] }", <object>{ {{}, {{ "1_78", {{ 'DATEI_1', 'C', 100, 0,'Anhang 1','Anhang1' }}, nil, nil }}} },"DATEI_1", typeof(string)) // FAIL - due to ARRAY:__SetElement() bug
//        TestMacro(mc, e"{|a| a[2,2,2,2,2] := 12, a[2,2,2,2,2] }", <object>{ {1,{1,{1,{1,{1, 3}}}}} }, 12 , typeof(int)) // FAIL - due to ARRAY:__SetElement() bug
        TestMacro(mc, e"{|a| a:ToString() }", Args(8), "8", typeof(string)) // FAIL - String:ToString() is overloaded!
        TestMacro(mc, e"{|a,b| a $ b}", Args("est", "test"), true, typeof(boolean))
        TestMacro(mc, e"{|a,b| a $ b}", Args("test", "est"), false, typeof(boolean))
        TestMacro(mc, e"{|a,b| sizeof(int) }", Args(), sizeof(int), typeof(dword))
        TestMacro(mc, e"{|a,b| sizeof(teststruct) }", Args(), sizeof(teststruct), typeof(dword))
        TestMacro(mc, e"{|a,b| sizeof(testclass.nested.child) }", Args(), sizeof(testclass.nested.child), typeof(dword))
//        TestMacro(mc, e"{|a,b| testclass.nested.child.haha }", Args(), 4321, typeof(int)) // FAIL - not supported
        TestMacro(mc, e"{|a,b| testclass.nested.fff }", Args(), 333, typeof(int))
        TestMacro(mc, e"{|a,b| default(testclass), sizeof(int) }", Args(), 4, typeof(dword))
        TestMacro(mc, e"{|a,b| testclass.nested.fff, sizeof(int) }", Args(), 4, typeof(dword))
        TestMacro(mc, e"{|a,b| 1 is int }", Args(), true, typeof(logic))
        TestMacro(mc, e"{|a,b| 1 is ValueType }", Args(), true, typeof(logic))
        TestMacro(mc, e"{|a,b| 1 is object }", Args(), true, typeof(logic))
        TestMacro(mc, e"{|a,b| 1 is real4 }", Args(), false, typeof(logic))
        TestMacro(mc, e"{|a,b| 1 is testclass }", Args(), false, typeof(logic))
        TestMacro(mc, e"{|a,b| 1 is teststruct }", Args(), false, typeof(logic))
        TestMacro(mc, e"{|a,b| 1 is testclass.nested }", Args(), false, typeof(logic))
        TestMacro(mc, e"{|a,b| testclass{} is testclass.nested.fff }", Args(), null, null, ErrorCode.NotAType)
        TestMacro(mc, e"{|a,b| testclass{} is testclass }", Args(), true, typeof(logic))
        TestMacro(mc, e"{|a,b| testclass{} is testclass.nested }", Args(), false, typeof(logic))
        TestMacro(mc, e"{|a,b| testclass{} is int }", Args(), false, typeof(logic))
        TestMacro(mc, e"{|a,b| sizeof(testclass.nested.fff) }", Args(), null, null, ErrorCode.NotAType)
        TestMacro(mc, e"{|a,b| default(testclass.nested.fff) }", Args(), null, null, ErrorCode.NotAType)
        TestMacro(mc, e"{|a,b| (testclass.nested.fff)123 }", Args(), null, null, ErrorCode.NotAType)
        TestMacro(mc, e"{|a,b| (testclass.nested)123 }", Args(), null, null, ErrorCode.NoConversion)
        TestMacro(mc, e"{|a,b| int is ValueType }", Args(), null, null, ErrorCode.NotAnExpression)
        TestMacro(mc, e"{|a,b| int }", Args(), null, null, ErrorCode.NotAnExpression)
        TestMacro(mc, e"{|a,b| U(int) }", Args(), null, null, ErrorCode.NotAnExpression)
//        TestMacro(mc, "U", Args(), null, null, ErrorCode.NotAnExpression) // It is treated as field/memvar
//        TestMacro(mc, e"{|a,b| asdgfafd(123) }", Args(), null, null, ErrorCode.NotAMethod)
        TestMacro(mc, e"{|a,b| testclass.nested(123) }", Args(), null, null, ErrorCode.NotAMethod)
        TestMacro(mc, e"{|a,b| Console.Write(null) }", Args(), null, typeof(object))
        TestMacro(mc, e"{|a,b| Console.Write() }", Args(), null, null, ErrorCode.NoSuitableOverload)
        TestMacro(mc, e"'AA' == U('A')", Args(), false, typeof(logic))
        TestMacro(mc, e"'AA' = U('A')", Args(), true, typeof(logic))
        TestMacro(mc, e"'AA' == 'A'", Args(), false, typeof(logic))
        TestMacro(mc, e"'AA' = 'A'", Args(), true, typeof(logic))
        TestMacro(mc, e"'AA' == (object)'A'", Args(), false, typeof(logic))
        TestMacro(mc, e"'AA' = (object)'A'", Args(), true, typeof(logic))
        TestMacro(mc, e"'AA' != U('A')", Args(), false, typeof(logic))
        TestMacro(mc, e"'AA' != 'A'", Args(), false, typeof(logic))
        TestMacro(mc, e"'AA' != (object)'A'", Args(), false, typeof(logic))
        TestMacro(mc, e"{|a,b| a > b}", Args("est","test"), false, typeof(logic))
        TestMacro(mc, e"{|a,b| a < b}", Args("est","test"), true, typeof(logic))
        TestMacro(mc, e"{|a| a < 'a'}", Args("est"), false, typeof(logic))
        TestMacro(mc, e"{|a| a > 'a'}", Args("est"), true, typeof(logic))
        TestMacro(mc, e"'A' > 'AA'", Args(), false, typeof(logic))
        TestMacro(mc, e"'A' < 'AA'", Args(), true, typeof(logic))
        TestMacro(mc, e"{|a| _NOT(a) }", Args(7), -8, typeof(int))
        TestMacro(mc, e"{|a| _AND(a,a) }", Args(7), 7, typeof(int))
        TestMacro(mc, e"{|a| _AND(a,a,a) }", Args(7), 7, typeof(int))
        TestMacro(mc, e"{|a| _AND(a,0,a) }", Args(7), 0, typeof(int))
        TestMacro(mc, e"_XOR(7,9)", Args(), 14, typeof(int))
        TestMacro(mc, e"_XOR(7,7)", Args(), 0, typeof(int))
        TestMacro(mc, e"_XOR(7,7,7)", Args(), 7, typeof(int))
        TestMacro(mc, e"{|a| (int)a }", Args(7), 7, typeof(int))
        TestMacro(mc, e"999999999999999999999999", Args(), null, null, ErrorCode.LiteralIntegerOverflow)
        TestMacro(mc, e"9.99999e999999999999999999", Args(), null, null, ErrorCode.LiteralFloatOverflow)
        TestMacro(mc, e"-tsi", Args(), null, null, ErrorCode.UnaryOperationNotFound)
        TestMacro(mc, e"tsi+1", Args(), null, null, ErrorCode.BinaryOperationNotFound)
        TestMacro(mc, e"tsi[2]", Args(), null, null, ErrorCode.NoConversion)
        TestMacro(mc, e"{|a,b| 1[2]}", Args(), null, null, ErrorCode.NoConversion)
        TestMacro(mc, "ArgCount(1,nil)", Args(), null, null, ErrorCode.BadNumArgs)
        TestMacro(mc, "ArgCount()", Args(), 0, typeof(int))
        TestMacro(mc, "{|a,b|ArgCount()}", Args(), 2, typeof(int))
        TestMacro(mc, "{|a|ArgCount()}", Args(1,2,3), 1, typeof(int))
        TestMacro(mc, "PCount(1,nil)", Args(), null, null, ErrorCode.BadNumArgs)
        TestMacro(mc, "PCount()", Args(), 0, typeof(int))
        TestMacro(mc, "{|a,b|PCount()}", Args(), 0, typeof(int))
        TestMacro(mc, "{|a|PCount()}", Args(1,2,3), 3, typeof(int))
        TestMacro(mc, e"_GetMParam(0)", Args(10, 20, 30.5), null, typeof(object))
        TestMacro(mc, e"_GetMParam(1)", Args(10, 20, 30.5), 10, typeof(int))
        TestMacro(mc, e"_GetMParam(2)", Args(10, 20, 30.5), 20, typeof(int))
        TestMacro(mc, e"_GetMParam(3)", Args(10, 20, 30.5), 30.5, typeof(real8))
        TestMacro(mc, e"_GetMParam(100)", Args(10, 20, 30.5), null, typeof(object))
        TestMacro(mc, e"_GetFParam(0)", Args(10, 20, 30.5), null, typeof(object))
        TestMacro(mc, e"_GetFParam(1)", Args(10, 20, 30.5), 10, typeof(int))
        TestMacro(mc, e"_GetFParam(2)", Args(10, 20, 30.5), 20, typeof(int))
        TestMacro(mc, e"_GetFParam(3)", Args(10, 20, 30.5), 30.5, typeof(real8))
        TestMacro(mc, e"_GetFParam(100)", Args(10, 20, 30.5), null, typeof(object))
        TestMacro(mc, e"testclass.nested.child.haha", Args(), testclass.nested.child.haha, typeof(testclass.nested.child))
        TestMacro(mc, e"testclass.nested.ttt", Args(), testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"testclass.nested.ccc", Args(), 456, typeof(int))
        TestMacro(mc, e"testclass.nested.eee", Args(), testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"(int)testclass.nested.eee", Args(), 1, typeof(int))
        TestMacro(mc, e"(testclass.nested.child)1", Args(), testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"(testclass.nested.child)1.5", Args(), testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"-(-testclass.nested.eee)", Args(), testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"-testclass.nested.eee", Args(), -1, typeof(testclass.nested.child))
        TestMacro(mc, e"2-testclass.nested.eee", Args(), testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"testclass.nested.eee+1", Args(), 2, typeof(testclass.nested.child))
        TestMacro(mc, e"testclass.nested.eee|testclass.nested.child.haha", Args(), testclass.nested.child.haha, typeof(testclass.nested.child))
        TestMacro(mc, e"testclass.nested.eee*2", Args(), 2, typeof(int))
        TestMacro(mc, e"testclass.nested.eee/1", Args(), 1, typeof(int))
        TestMacro(mc, e"1_000", Args(), 1000, typeof(int))
        TestMacro(mc, e"1_001.1_2", Args(), 1001.12, typeof(float))
        TestMacro(mc, e"1_001.1_2e1_2", Args(), 1001.12e12, typeof(float))
        TestMacro(mc, e"1000_", Args(), null, null, ErrorCode.InvalidNumber)
        TestMacro(mc, e"1_001.1_", Args(), null, null, ErrorCode.InvalidNumber)
        TestMacro(mc, e"1_001.1_2e1_", Args(), null, null, ErrorCode.InvalidNumber)
        TestMacro(mc, e"1_001.1_e1", Args(), null, null, ErrorCode.InvalidNumber)
        TestMacro(mc, e"123.45e0m", Args(), null, null, ErrorCode.Unexpected)
        TestMacro(mc, e"--testclass.nested.eee", Args(), null, null, ErrorCode.NoAccessMode)
        TestMacro(mc, e"testclass(9)", Args(), null, null, ErrorCode.NotAMethod)
        TestMacro(mc, e"{|a| a(12332) }", Args((@@Func<int,int>)I), 12332, typeof(int))
        TestMacro(mc, e"{|a|A*1_000", Args(123), 123000, typeof(int))
        TestMacro(mc, e"{|a| USUAL(-a) }", Args(1), -1, typeof(int))
        TestMacro(mc, e"{|a| (USUAL)(-a) }", Args(1), -1, typeof(int))
        TestMacro(mc, e"0.00001", Args(), 1e-5, typeof(float))
        TestMacro(mc, e"{|a,b,c|DoTest(a,b,c)}", Args(1, true, nil), 5, typeof(int))
        TestMacro(mc, e"{|a,b,c|DoTestC(a,b,c)}", Args(1, true, testclass{222}), 222, typeof(int))
        TestMacro(mc, e"{|a,b,c|DoTestS(a,b,c)}", Args(1, true, teststruct{222}), 222, typeof(int))
        TestMacro(mc, e"{|a| AScan(a, \"12\") }", Args({"135454","54376","123","53"}, nil), 3, typeof(dword))
        TestMacro(mc, e"{|a| ALen(a) }", Args({"1235454","54376","12","53"},nil), 4, typeof(dword))
        TestMacro(mc, e"{|a| (testclass)a }",Args(tci), tci, typeof(testclass))
        TestMacro(mc, e"{|a| ((int)a):GetHashCode() }", Args(8), 8, typeof(int))
        TestMacro(mc, e"{|a| a:GetHashCode() }", Args(8), 8, typeof(int))
        TestMacro(mc, e"{|a| a:GetHashCode() }", Args(tci), tci:GetHashCode(), typeof(int))
        TestMacro(mc, e"{|a| a:GetHashCode() }", Args(tsi), tsi:GetHashCode(), typeof(int))
        TestMacro(mc, e"{|| tci:GetHashCode() }", Args(), tci:GetHashCode(), typeof(int))
        TestMacro(mc, e"{|| tsi:GetHashCode() }", Args(), tsi:GetHashCode(), typeof(int))
        TestMacro(mc, e"{|a| ((int)a):ToString() }", Args(8), "8", typeof(string))
        TestMacro(mc, e"{|a| a:ToString() }", Args(8), "8", typeof(string))
        TestMacro(mc, e"{|a| a:ToString() }", Args(tci), "testclass", typeof(string))
        TestMacro(mc, e"{|a| a:ToString() }", Args(tsi), "teststruct", typeof(string))
        TestMacro(mc, e"{|| tci:ToString() }", Args(), "testclass", typeof(string))
        TestMacro(mc, e"{|| tsi:ToString() }", Args(), "teststruct", typeof(string))
        TestMacro(mc, e"{|a| a:UString() }", Args(tci), "1", typeof(string))
        TestMacro(mc, e"{|a| a:UString() }", Args(tsi), "1", typeof(string))
        TestMacro(mc, e"{|| testbase{}:FString() }", Args(), "base", typeof(string))
        TestMacro(mc, e"{|| ((testbase)tci):FString() }", Args(), "1", typeof(string))
        TestMacro(mc, e"{|| tsi:FString() }", Args(), "1", typeof(string))
        TestMacro(mc, e"{|| tci:FString('fff') }", Args(), "fff1", typeof(string))
        TestMacro(mc, e"{|| tsi:FString('fff') }", Args(), "fff1", typeof(string))
        TestMacro(mc, e"{|| testbase{}:NString((byte)1) }", Args(), "base", typeof(string))
        TestMacro(mc, e"{|| testclass{}:NString((byte)1) }", Args(), "child", typeof(string))
        TestMacro(mc, e"{|| ((testbase)testclass{}):NString((byte)1) }", Args(), "base", typeof(string))
        TestMacro(mc, e"{|| testclass{}:BString() }", Args(), "bbase", typeof(string))
        TestMacro(mc, "{|abc| Chr(abc) + 'B'}", Args(65), "AB", typeof(string))
        TestMacro(mc, "{|abc| Chr(65) + 'B'}", Args(), "AB", typeof(string))
        TestMacro(mc, '{|abc| Chr(65) + "BB"}', Args(), "ABB", typeof(string))
        TestMacro(mc, "{|abc| Chr(65):toString() + 'B'}", Args(), "AB", typeof(string))
        TestMacro(mc, e"{|abc| (usual)\"ABC\" + Chr(123)}", Args(), "ABC"+Chr(123), typeof(string)) 
        TestMacro(mc, e"0x1234", Args(), 0x1234, typeof(int))
        TestMacro(mc, e"0b110011", Args(), 0b110011, typeof(int))
        TestMacro(mc, e"0xFFFF", Args(), 0xFFFF, typeof(int))
        TestMacro(mc, e"0XFFFF", Args(), 0xFFFF, typeof(int))
        TestMacro(mc, e"0xffff", Args(), 0xFFFF, typeof(int))
        TestMacro(mc, "1+1 ", Args(), 2, typeof(int))
        TestMacro(mc, e"{|a|Len(a) == 4}", Args("test"), true, typeof(logic))
        TestMacro(mc, e"'a' $ 'b'", Args(), false, typeof(logic))
        TestMacro(mc, e"'a' $ 'a'", Args(), true, typeof(logic))
        TestMacro(mc, e"iif('a' $ 'a' , TRUE, FALSE)", Args(), true, typeof(logic))
        TestMacro(mc, e"iif('a' $ 'a' , 1, 2)", Args(), 1, typeof(int))
        TestMacro(mc, e"right('abcdef',3)", Args(), "def", typeof(string))
        TestMacro(mc, e"left('abcdef',3)", Args(), "abc", typeof(string))
        TestMacro(mc, e"left('abcdef',3+1)", Args(), "abcd", typeof(string))
        TestMacro(mc, e"{|o|o:fld := ctest{1,0}}", Args(ctest{0,0}), ctest{1,0}, typeof(ctest))

        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsField
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGet, "MyFieldGet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSet, "MyFieldSet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGetWa, "MyFieldGetWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSetWa, "MyFieldSetWa")
        TestMacro(mc, "U", Args(), "FieldGet(U)", typeof(string))
        TestMacro(mc, e"{|| NIKOS}", Args(), "FieldGet(NIKOS)", typeof(string))
        TestMacro(mc, e"{|| NIKOS := \"123\"}", Args(), "FieldSet(NIKOS):123", typeof(string))
        TestMacro(mc, e"{|| _FIELD->NIKOS}", Args(), "FieldGet(NIKOS)", typeof(string))
        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS}", Args(), "FieldGet(BASE,NIKOS)", typeof(string))
        TestMacro(mc, e"{|| BASE->NIKOS}", Args(), "FieldGet(BASE,NIKOS)", typeof(string))
        TestMacro(mc, e"{|| _FIELD->NIKOS := \"123\"}", Args(), "FieldSet(NIKOS):123", typeof(string))
        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS := \"123\"}", Args(), "FieldSet(BASE,NIKOS):123", typeof(string))
        TestMacro(mc, e"{|| BASE->NIKOS := \"123\"}", Args(), "FieldSet(BASE,NIKOS):123", typeof(string))

        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsFieldOrMemvar
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___VarGet, "MyVarGet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___VarPut, "MyVarPut")
        TestMacro(mc, e"{|| NIKOS}", Args(), "VarGet(NIKOS)", typeof(string))
        TestMacro(mc, e"{|| NIKOS := \"123\"}", Args(), "VarPut(NIKOS):123", typeof(string))
        TestMacro(mc, e"{|a,b| asdgfafd(123) }", Args(), null, null, ErrorCode.NotAMethod)

        Console.WriteLine("Total pass: {0}/{1}", TotalSuccess, TotalTests)
        return

    function RunPerf(mc as XSharp.Runtime.MacroCompiler, src as string) as void
        TestMacroCompiler(mc, src, 15, true, false)
        TestMacroCompiler(mc, src, 15, true, true)
        TestMacroCompiler(mc, src, 100000, false, false)
        TestMacroCompiler(mc, src, 100000, false, true)
        return

    function TestParse(mc as XSharp.Runtime.MacroCompiler, src as string, val as string) as logic
        TotalTests += 1
        Console.Write("Test: '{0}' ", src)
        var res := mc:compiler:Parse(src):ToString()
        if res = val
            TotalSuccess += 1
            Console.WriteLine("[OK]")
            return true
        else
            TotalFails += 1
            Console.WriteLine("[FAIL] ({0} != {1})", res, val)
        end
        return false

    function TestMacro(mc as XSharp.Runtime.MacroCompiler, src as string, args as object[], expect as usual, t as Type, ec := ErrorCode.NoError as ErrorCode) as logic
        try
            TotalTests += 1
            Console.Write("Test: '{0}' ", src)
            var cb := mc:Compile(src)
            var res := cb:EvalBlock(args)
            local match as logic
            if IsArray(expect)
                match := ALen(expect) = ALen(res)
                for var i := 1 to ALen(expect)
                    if expect[i] != ((array)res)[i]
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
            if (ec == ErrorCode.NoError) .and. (match) .and. ((t == null) || (t == res?:GetType()) || (t == typeof(object) .and. res == null .and. expect == null))
                TotalSuccess += 1
                Console.WriteLine("[OK]")
                return true
            else
                TotalFails += 1
                Console.WriteLine("[FAIL] (res = {0}, type = {1}, no error)", res, res?:GetType())
            end
            return false
        catch e as CompilationError
            if e:@@Code == ec
                TotalSuccess += 1
                Console.WriteLine("[OK]")
                return true
            else
                TotalFails += 1
                Console.WriteLine("[FAIL] ({0})", e:Message)
            end
            return false
/*        catch e as Exception
            TotalFails += 1
            Console.WriteLine("[FAIL] (Exception: {0})", e:Message)
            return false*/
        end

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

