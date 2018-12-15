using System
using System.Collections.Generic
using System.Linq
using System.Text
using XSharp.MacroCompiler

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

class testclass
    class nested
        enum child
            haha := 4321
            blabla := 1
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

begin namespace MacroCompilerTest
    using XSharp.Runtime
    using XSharp.MacroCompiler

	function Start() as void
	    SetMacroCompiler(typeof(XSharp.Runtime.MacroCompiler))

        ReportMemory("initial")
        var mc := CreateMacroCompiler()

        //ParseMacro(mc, e"{|a,b| +a[++b] += 100, a[2]}")

        //EvalMacro(mc, e"{|a,b| a[++b] += 100, a[2]}", {1,2,3}, 1)
        //EvalMacro(mc, e"{|a| (testclass)a }",tci) // FAIL - should work (TODO: implement type casts)
        //EvalMacro(mc, e"{|a,b| asdgfafd(123) }") // FAIL - error message is OK but TestMacro() fails
        EvalMacro(mc, e"testclass.nested.eee")
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
        catch e as XSharp.MacroCompiler.CompilationError
            Console.WriteLine("{0}",e:Message)
            return nil
        end

    global TotalFails := 0 as int
    global TotalTests := 0 as int
    global TotalSuccess := 0 as int

    function RunTests(mc as XSharp.Runtime.MacroCompiler) as void
        Console.WriteLine("Running tests ...")

        TestParse(mc, e"{|a,b| +a[++b] += 100, a[2]}", "{|a, b|((+a((++b)))+='100'), a('2')}")
        TestMacro(mc, e"{|a,b| asdgfafd(123) }", <object>{}, null, null,ErrorCode.NotAMethod)

        XSharp.Runtime.MacroCompiler.Options:UndeclaredVariableResolution := VariableResolution.Error
        TestMacro(mc, e"{|a,b| testtest__() }", <object>{1,2,3}, null, null, ErrorCode.IdentifierNotFound)

        XSharp.Runtime.MacroCompiler.Options:UndeclaredVariableResolution := VariableResolution.GenerateLocal
        TestMacro(mc, e"{|a| a() }", <object>{(@@Func<int>){ => 1234}}, 1234, typeof(int))
        TestMacro(mc, "#HELLo", <OBJECT>{}, #hello, typeof(symbol))
        TestMacro(mc, "#HELLo + #World", <OBJECT>{}, #hello + #world, typeof(string))
        TestMacro(mc, e"#HELLo + \"world\"", <OBJECT>{}, #hello + "world", typeof(string))
        TestMacro(mc, e"\"Hello\" + #world", <OBJECT>{}, "Hello" + #world, typeof(string))
        TestMacro(mc, "U(12345)", <OBJECT>{}, 12345, typeof(int))
        TestMacro(mc, "U(U(12345)-1)", <OBJECT>{}, 12344, typeof(int))
        TestMacro(mc, "I(123+45)", <OBJECT>{}, 123+45, typeof(int))
        TestMacro(mc, "R(123)", <OBJECT>{}, 123, typeof(real8))
        TestMacro(mc, "R(123.456)", <OBJECT>{}, 123.456, typeof(real8))
        TestMacro(mc, "U(123.456)", <OBJECT>{}, 123.456, typeof(float))
        TestMacro(mc, "123.456", <OBJECT>{}, 123.456, typeof(float))
        TestMacro(mc, "123.456s", <OBJECT>{}, 123.456s, typeof(real4))
        TestMacro(mc, "123.456d", <OBJECT>{}, 123.456d, typeof(real8))
        TestMacro(mc, "123.450m", <OBJECT>{}, 123.450m, typeof(decimal))
        TestMacro(mc, "123.450m+321", <OBJECT>{}, 444.450m, typeof(decimal))
        TestMacro(mc, "123.450m+321.05", <OBJECT>{}, 444.500m, typeof(decimal))
        TestMacro(mc, "{|a,b,c|a := b := 1343+1}", <OBJECT>{}, 1343+1, typeof(int))
        TestMacro(mc, "{|a,b,c|}", <OBJECT>{}, null, typeof(object))
        TestMacro(mc, "{|a,b,c|1234}", <OBJECT>{}, 1234, typeof(int))
        TestMacro(mc, "1234", <OBJECT>{}, 1234, typeof(int))
        TestMacro(mc, "12 == 12", <OBJECT>{}, true, typeof(logic))
        TestMacro(mc, "", <OBJECT>{}, null, typeof(object))
        TestMacro(mc, "2018.12.31", <OBJECT>{}, 2018.12.31, typeof(date))
        TestMacro(mc, "2018.1.1", <OBJECT>{}, 2018.1.1, typeof(date))
        TestMacro(mc, "2018.12.31 == 2018.12.31", <OBJECT>{}, true, typeof(logic))
        TestMacro(mc, "2018.12.31 = 2018.12.31", <OBJECT>{}, true, typeof(logic))
        TestMacro(mc, "2018.12.31 = 2018.1.1", <OBJECT>{}, false, typeof(logic))
        TestMacro(mc, "2018.12.31 != 2018.12.31", <OBJECT>{}, false, typeof(logic))
        TestMacro(mc, "2018.12.31 != 2018.1.1", <OBJECT>{}, true, typeof(logic))
        TestMacro(mc, "null", <OBJECT>{}, null, typeof(object))
        TestMacro(mc, "null_object", <OBJECT>{}, null_object, null)
        TestMacro(mc, "null_string", <OBJECT>{}, null_string, null)
        TestMacro(mc, "null_psz = psz._NULL_PSZ", <OBJECT>{}, true, typeof(logic))
        TestMacro(mc, "null_symbol", <OBJECT>{}, null_symbol, typeof(symbol))
        TestMacro(mc, "null_date", <OBJECT>{}, null_date, typeof(date))
        TestMacro(mc, "null_codeblock", <OBJECT>{}, null_codeblock, null)
        TestMacro(mc, "null_ptr", <OBJECT>{}, null_ptr, null)
        TestMacro(mc, "{|a,b,c|a := b := 1343, c := a + 1, a+b-c/2}", <OBJECT>{}, 1343+1343-(1343+1)/2, typeof(int))
        TestMacro(mc, "{|a|a := 1343, a += 1}", <OBJECT>{}, 1343+1, typeof(int))
        TestMacro(mc, "{|a|a := -1343, a := -a}", <OBJECT>{}, 1343, typeof(int))
        TestMacro(mc, "{|a|a := 8, ++a, ++a}", <OBJECT>{123}, 10, typeof(int))
        TestMacro(mc, "{|a|a := 8, ++a, a++, a++}", <OBJECT>{123}, 10, typeof(int))
        TestMacro(mc, "{|a|++a, a++, a++}", <OBJECT>{8}, 10, typeof(int))
        TestMacro(mc, "{|a| a++, U(a++), a++}", <OBJECT>{8}, 10, typeof(int))
        TestMacro(mc, e"{|a| a := \"abc\" + \"def\"}", <OBJECT>{8}, "abcdef", typeof(string))
        TestMacro(mc, e"{|a| \"abc\" == \"def\"}", <OBJECT>{8}, false, typeof(logic))
        TestMacro(mc, e"{|a| \"abc\" = \"abc\"}", <OBJECT>{8}, true, typeof(logic))
        TestMacro(mc, e"{|a| \"abc\" != \"abc\"}", <OBJECT>{8}, false, typeof(logic))
        TestMacro(mc, e"{|a| a := \"abc\", a == \"abc\"}", <OBJECT>{8}, true, typeof(logic))
        TestMacro(mc, e"{|a| a := \"abc\", a + \"def\"}", <OBJECT>{8}, "abcdef", typeof(string))
        TestMacro(mc, e"{|a| 0 == 0}", <OBJECT>{8}, true, typeof(logic))
        TestMacro(mc, e"{|a| 0 != 0}", <OBJECT>{8}, false, typeof(logic))
        TestMacro(mc, e"{|a| (0 > 1) .and. (0 < 1) }", <OBJECT>{8}, false, typeof(logic))
        TestMacro(mc, e"{|a| a := \"qwerty\", a:Length }", <OBJECT>{8}, 6, typeof(int))
        TestMacro(mc, e"{|a| a := default(int) }", <OBJECT>{8}, 0, typeof(int))
        TestMacro(mc, e"{|a| a := default(string) }", <OBJECT>{8}, null, typeof(object))
        TestMacro(mc, e"{|a| a := default(usual) }", <OBJECT>{8}, null, typeof(object))
        TestMacro(mc, e"{|a| a := U(1234+1), a }", <OBJECT>{8}, 1234+1, typeof(int))
        TestMacro(mc, e"{|a| UU := U(1234+1), UU }", <OBJECT>{8}, 1234+1, typeof(int))
        TestMacro(mc, e"{|a| a := \"abcdef\", a:ToUpperInvariant() }", <OBJECT>{8}, "ABCDEF", typeof(string))
        TestMacro(mc, e"{|a| a := NIL }", <OBJECT>{8}, null, typeof(object))
        TestMacro(mc, e"{|| I3(4,4,4) }", <OBJECT>{}, 12, typeof(int))
        TestMacro(mc, e"{|a| a := I3(4,4,) }", <OBJECT>{}, 11, typeof(int))
        TestMacro(mc, e"{|a| a := I3(4,,4) }", <OBJECT>{}, 10, typeof(int))
        TestMacro(mc, e"{|a| a := I3(,4,4) }", <OBJECT>{}, 9, typeof(int))
        TestMacro(mc, e"{|a| a := I3(,,) }", <OBJECT>{}, 6, typeof(int))
        TestMacro(mc, e"{|a| a := I3(4,4) }", <OBJECT>{}, 11, typeof(int))
        TestMacro(mc, e"{|a| a := I3(4) }", <OBJECT>{}, 9, typeof(int))
        TestMacro(mc, e"{|a| a := I3() }", <OBJECT>{}, 6, typeof(int))
        TestMacro(mc, e"{|a| a := I0() }", <OBJECT>{}, 123, typeof(int))
        TestMacro(mc, e"{|a| a := CC(1,2,3) }", <OBJECT>{}, 6, typeof(int))
        TestMacro(mc, e"{|a| a := CC(1,2,3,4) }", <OBJECT>{}, 6, typeof(int))
        TestMacro(mc, e"{|a| a := CC() }", <OBJECT>{}, 0, typeof(int))
        TestMacro(mc, e"{|a| a := CC(1,2) }", <OBJECT>{}, 3, typeof(int))
        TestMacro(mc, e"{|a| a := CC(,1,2) }", <OBJECT>{}, 3, typeof(int))
        TestMacro(mc, e"{|a| a := CC(1,,2) }", <OBJECT>{}, 3, typeof(int))
        TestMacro(mc, e"{|a| a := U({1,2,3}) }", <OBJECT>{}, {1,2,3}, typeof(array))
        TestMacro(mc, e"{|a| a := U({1,,2,,}) }", <OBJECT>{}, {1,NIL,2,NIL,NIL}, typeof(array))
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
        TestMacro(mc, e"{|a| a := testclass{}, a:prop }", <OBJECT>{}, 0, typeof(int))
        TestMacro(mc, e"{|a| a := testclass{222}, a:prop }", <OBJECT>{}, 222, typeof(int))
        TestMacro(mc, e"{|a| a := teststruct{}, a:prop }", <OBJECT>{}, 0, typeof(int))
        TestMacro(mc, e"{|a| a := teststruct{222}, a:prop }", <OBJECT>{}, 222, typeof(int))
        TestMacro(mc, e"{|a| a := testclass{}, a:v1 := 1 }", <OBJECT>{}, 1, typeof(int))
        TestMacro(mc, e"{|a| a := testclass{222}, a:v1 }", <OBJECT>{}, 222, typeof(int))
        TestMacro(mc, e"{|a| a := teststruct{}, a:v1 := 1 }", <OBJECT>{}, 1, typeof(int))
        TestMacro(mc, e"{|a| a := teststruct{222}, a:v1 }", <OBJECT>{}, 222, typeof(int))
        TestMacro(mc, e"{|a| a := testclass{}, a:prop := 111 }", <OBJECT>{}, 111, typeof(int))
        TestMacro(mc, e"{|a,b| b := testclass{}, b:prop := a, ++b:prop }", <OBJECT>{55}, 56, typeof(int))
        TestMacro(mc, e"{|| tsi:v1 }", <OBJECT>{}, 1, typeof(int))
        TestMacro(mc, e"{|| ++tsi:v1 }", <OBJECT>{}, 2, typeof(int))
//        TestMacro(mc, e"{|| tsi:v1 := 10, tsi:v1 }", <OBJECT>{}, 10, typeof(int)) // FAIL because tsi is boxed by value for IVarPut()
        TestMacro(mc, e"{|| ++tsi:prop }", <OBJECT>{}, 2, typeof(int))
        TestMacro(mc, e"{|| tci:v1 }", <OBJECT>{}, 1, typeof(int))
        TestMacro(mc, e"{|| ++tci:v1 }", <OBJECT>{}, 2, typeof(int))
        TestMacro(mc, e"{|| ++tci:prop }", <OBJECT>{}, 2, typeof(int))
        TestMacro(mc, e"{|| tci:v1 := 10, tci:v1++, tci:v1 }", <OBJECT>{}, 11, typeof(int))
        TestMacro(mc, e"{|a| IIF(a>10,123,1.23) }", <OBJECT>{100}, 123, typeof(float))
        TestMacro(mc, e"{|a| IIF(a>10,123,1.23) }", <OBJECT>{1}, 1.23, typeof(float))
        TestMacro(mc, e"{|a| IIF(a>10,1) }", <OBJECT>{100}, 1, typeof(int))
        TestMacro(mc, e"{|a| IIF(a>10,,1) }", <OBJECT>{100}, null, typeof(object))
        TestMacro(mc, e"{|a| (float)++a/2 }", <OBJECT>{2}, 1.5, typeof(float))
        TestMacro(mc, e"{|a| a := {1,2,3,4}, a[1] := 10, a[1] + a[2] }", <OBJECT>{}, 12, typeof(int))
        TestMacro(mc, e"{|a| a := {1,2,3,4}, a[1] += 10, a[1] }", <OBJECT>{}, 11, typeof(int))
        TestMacro(mc, "{|a|a := 8, a := 8**a}", <OBJECT>{123}, 16777216, typeof(float))
        TestMacro(mc, "I((int)123.456)", <OBJECT>{}, 123, typeof(int))
        TestMacro(mc, "{|a| b := 8, c := b**a, c}", <OBJECT>{8}, 16777216, typeof(float))
        TestMacro(mc, "{|a,b,c|a.and.b.or..not.c}", <OBJECT>{true,false,true}, false, typeof(logic))
        TestMacro(mc, "{|a| a := U({1,2,3", <OBJECT>{}, {1,2,3}, typeof(array))
//        TestMacro(mc, e"{|| _FIELD->NIKOS}", <OBJECT>{}, nil, typeof(object))
//        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS}", <OBJECT>{}, nil, typeof(object))
//        TestMacro(mc, e"{|| BASE->NIKOS}", <OBJECT>{}, nil, typeof(object))
//        TestMacro(mc, e"{|| _FIELD->NIKOS := 123}", <OBJECT>{}, 123, typeof(object))
//        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS := 123}", <OBJECT>{}, 123, typeof(object))
//        TestMacro(mc, e"{|| BASE->NIKOS := 123}", <OBJECT>{}, 123, typeof(object))
        TestMacro(mc, e"{|a,b| a[++b] += 100, a[2]}", <OBJECT>{{1,2,3}, 1}, 102, typeof(int))
        TestMacro(mc, e"_chr(65)", <object>{}, 65, typeof(char))
        TestMacro(mc, e"chr(65)", <object>{}, 65, typeof(char))
        TestMacro(mc, e"char(65)", <object>{}, 65, typeof(char))
        TestMacro(mc, e"slen(\"hello\")", <object>{}, 5, typeof(dword))
        TestMacro(mc, e"{|v| v[2,1,2,1,1] }", <OBJECT>{ {{}, {{ "1_78", {{ 'DATEI_1', 'C', 100, 0,'Anhang 1','Anhang1' }}, nil, nil }}} },"DATEI_1", typeof(string))
//        TestMacro(mc, e"{|v| v[2,1,2,1,1] := 'TEST', v[2,1,2,1,1] }", <OBJECT>{ {{}, {{ "1_78", {{ 'DATEI_1', 'C', 100, 0,'Anhang 1','Anhang1' }}, nil, nil }}} },"DATEI_1", typeof(string)) // FAIL - due to ARRAY:__SetElement() bug
//        TestMacro(mc, e"{|a| a[2,2,2,2,2] := 12, a[2,2,2,2,2] }", <object>{ {1,{1,{1,{1,{1, 3}}}}} }, 12 , typeof(int)) // FAIL - due to ARRAY:__SetElement() bug
//        TestMacro(mc, e"{|a| a:ToString() }", <OBJECT>{8}, "8", typeof(string)) // FAIL - String:ToString() is overloaded!
        TestMacro(mc, e"{|a,b| a $ b}", <object>{"est", "test"}, true, typeof(boolean))
        TestMacro(mc, e"{|a,b| a $ b}", <object>{"test", "est"}, false, typeof(boolean))
        TestMacro(mc, e"{|a,b| sizeof(int) }", <object>{}, sizeof(int), typeof(dword))
        TestMacro(mc, e"{|a,b| sizeof(teststruct) }", <object>{}, sizeof(teststruct), typeof(dword))
        TestMacro(mc, e"{|a,b| sizeof(testclass.nested.child) }", <object>{}, sizeof(testclass.nested.child), typeof(dword))
//        TestMacro(mc, e"{|a,b| testclass.nested.child.haha }", <object>{}, 4321, typeof(int)) // FAIL - not supported
        TestMacro(mc, e"{|a,b| testclass.nested.fff }", <object>{}, 333, typeof(int))
        TestMacro(mc, e"{|a,b| default(testclass), sizeof(int) }", <object>{}, 4, typeof(dword))
        TestMacro(mc, e"{|a,b| testclass.nested.fff, sizeof(int) }", <object>{}, 4, typeof(dword))
        TestMacro(mc, e"{|a,b| 1 is int }", <object>{}, true, typeof(logic))
        TestMacro(mc, e"{|a,b| 1 is ValueType }", <object>{}, true, typeof(logic))
        TestMacro(mc, e"{|a,b| 1 is object }", <object>{}, true, typeof(logic))
        TestMacro(mc, e"{|a,b| 1 is real4 }", <object>{}, false, typeof(logic))
        TestMacro(mc, e"{|a,b| 1 is testclass }", <object>{}, false, typeof(logic))
        TestMacro(mc, e"{|a,b| 1 is teststruct }", <object>{}, false, typeof(logic))
        TestMacro(mc, e"{|a,b| 1 is testclass.nested }", <object>{}, false, typeof(logic))
        TestMacro(mc, e"{|a,b| testclass{} is testclass.nested.fff }", <object>{}, null, null, ErrorCode.NotAType)
        TestMacro(mc, e"{|a,b| testclass{} is testclass }", <object>{}, true, typeof(logic))
        TestMacro(mc, e"{|a,b| testclass{} is testclass.nested }", <object>{}, false, typeof(logic))
        TestMacro(mc, e"{|a,b| testclass{} is int }", <object>{}, false, typeof(logic))
        TestMacro(mc, e"{|a,b| sizeof(testclass.nested.fff) }", <object>{}, null, null, ErrorCode.NotAType)
        TestMacro(mc, e"{|a,b| default(testclass.nested.fff) }", <object>{}, null, null, ErrorCode.NotAType)
        TestMacro(mc, e"{|a,b| (testclass.nested.fff)123 }", <object>{}, null, null, ErrorCode.NotAType)
        TestMacro(mc, e"{|a,b| (testclass.nested)123 }", <object>{}, null, null, ErrorCode.NoConversion)
        TestMacro(mc, e"{|a,b| int is ValueType }", <object>{}, null, null, ErrorCode.NotAnExpression)
        TestMacro(mc, e"{|a,b| int }", <object>{}, null, null, ErrorCode.NotAnExpression)
        TestMacro(mc, e"{|a,b| U(int) }", <object>{}, null, null, ErrorCode.NotAnExpression)
        TestMacro(mc, "U", <object>{}, null, null, ErrorCode.NotAnExpression)
//        TestMacro(mc, e"{|a,b| asdgfafd(123) }", <object>{}, null, null, ErrorCode.NotAMethod)
        TestMacro(mc, e"{|a,b| testclass.nested(123) }", <object>{}, null, null, ErrorCode.NotAMethod)
        TestMacro(mc, e"{|a,b| Console.Write(null) }", <object>{}, null, typeof(object))
        TestMacro(mc, e"{|a,b| Console.Write() }", <object>{}, null, null, ErrorCode.NoSuitableOverload)
        TestMacro(mc, e"'AA' == U('A')", <object>{}, false, typeof(logic))
        TestMacro(mc, e"'AA' = U('A')", <object>{}, true, typeof(logic))
        TestMacro(mc, e"'AA' == 'A'", <object>{}, false, typeof(logic))
        TestMacro(mc, e"'AA' = 'A'", <object>{}, true, typeof(logic))
        TestMacro(mc, e"'AA' == (object)'A'", <object>{}, false, typeof(logic))
        TestMacro(mc, e"'AA' = (object)'A'", <object>{}, true, typeof(logic))
        TestMacro(mc, e"'AA' != U('A')", <object>{}, false, typeof(logic))
        TestMacro(mc, e"'AA' != 'A'", <object>{}, false, typeof(logic))
        TestMacro(mc, e"'AA' != (object)'A'", <object>{}, false, typeof(logic))
        TestMacro(mc, e"{|a,b| a > b}", <object>{"est","test"}, false, typeof(logic))
        TestMacro(mc, e"{|a,b| a < b}", <object>{"est","test"}, true, typeof(logic))
        TestMacro(mc, e"{|a| a < 'a'}", <object>{"est"}, false, typeof(logic))
        TestMacro(mc, e"{|a| a > 'a'}", <object>{"est"}, true, typeof(logic))
        TestMacro(mc, e"'A' > 'AA'", <object>{}, false, typeof(logic))
        TestMacro(mc, e"'A' < 'AA'", <object>{}, true, typeof(logic))
        TestMacro(mc, e"{|a| _NOT(a) }", <object>{7}, -8, typeof(int))
        TestMacro(mc, e"{|a| _AND(a,a) }", <object>{7}, 7, typeof(int))
        TestMacro(mc, e"{|a| _AND(a,a,a) }", <object>{7}, 7, typeof(int))
        TestMacro(mc, e"{|a| _AND(a,0,a) }", <object>{7}, 0, typeof(int))
        TestMacro(mc, e"_XOR(7,9)", <object>{}, 14, typeof(int))
        TestMacro(mc, e"_XOR(7,7)", <object>{}, 0, typeof(int))
        TestMacro(mc, e"_XOR(7,7,7)", <object>{}, 7, typeof(int))
        TestMacro(mc, e"{|a| (int)a }", <object>{7}, 7, typeof(int))
        TestMacro(mc, e"999999999999999999999999", <object>{}, null, null, ErrorCode.LiteralIntegerOverflow)
        TestMacro(mc, e"9.99999e999999999999999999", <object>{}, null, null, ErrorCode.LiteralFloatOverflow)
        TestMacro(mc, e"-tsi", <object>{}, null, null, ErrorCode.UnaryOperationNotFound)
        TestMacro(mc, e"tsi+1", <object>{}, null, null, ErrorCode.BinaryOperationNotFound)
        TestMacro(mc, e"tsi[2]", <object>{}, null, null, ErrorCode.NoConversion)
        TestMacro(mc, e"{|a,b| 1[2]}", <object>{}, null, null, ErrorCode.NoConversion)
        TestMacro(mc, "ArgCount(1,nil)", <object>{}, null, null, ErrorCode.BadNumArgs)
        TestMacro(mc, "ArgCount()", <object>{}, 0, typeof(int))
        TestMacro(mc, "{|a,b|ArgCount()}", <object>{}, 2, typeof(int))
        TestMacro(mc, "{|a|ArgCount()}", <object>{1,2,3}, 1, typeof(int))
        TestMacro(mc, "PCount(1,nil)", <object>{}, null, null, ErrorCode.BadNumArgs)
        TestMacro(mc, "PCount()", <object>{}, 0, typeof(int))
        TestMacro(mc, "{|a,b|PCount()}", <object>{}, 0, typeof(int))
        TestMacro(mc, "{|a|PCount()}", <object>{1,2,3}, 3, typeof(int))
        TestMacro(mc, e"_GetMParam(0)", <object>{10, 20, 30.5}, null, typeof(object))
        TestMacro(mc, e"_GetMParam(1)", <object>{10, 20, 30.5}, 10, typeof(int))
        TestMacro(mc, e"_GetMParam(2)", <object>{10, 20, 30.5}, 20, typeof(int))
        TestMacro(mc, e"_GetMParam(3)", <object>{10, 20, 30.5}, 30.5, typeof(real8))
        TestMacro(mc, e"_GetMParam(100)", <object>{10, 20, 30.5}, null, typeof(object))
        TestMacro(mc, e"_GetFParam(0)", <object>{10, 20, 30.5}, null, typeof(object))
        TestMacro(mc, e"_GetFParam(1)", <object>{10, 20, 30.5}, 10, typeof(int))
        TestMacro(mc, e"_GetFParam(2)", <object>{10, 20, 30.5}, 20, typeof(int))
        TestMacro(mc, e"_GetFParam(3)", <object>{10, 20, 30.5}, 30.5, typeof(real8))
        TestMacro(mc, e"_GetFParam(100)", <object>{10, 20, 30.5}, null, typeof(object))
        TestMacro(mc, e"testclass.nested.child.haha", <object>{}, testclass.nested.child.haha, typeof(testclass.nested.child))
        TestMacro(mc, e"testclass.nested.ttt", <object>{}, testclass.nested.child.blabla, typeof(testclass.nested.child))
        TestMacro(mc, e"testclass.nested.ccc", <object>{}, 456, typeof(int))
        TestMacro(mc, e"testclass.nested.eee", <object>{}, testclass.nested.child.blabla, typeof(testclass.nested.child))

//        XSharp.Runtime.MacroCompiler.Options:UndeclaredVariableResolution := VariableResolution.TreatAsField
//        TestMacro(mc, e"{|| NIKOS}", <OBJECT>{}, nil, typeof(usual))
//        TestMacro(mc, e"{|| NIKOS := 123}", <OBJECT>{}, 123, typeof(int))

        XSharp.Runtime.MacroCompiler.Options:UndeclaredVariableResolution := VariableResolution.TreatAsFieldOrMemvar
        Compilation.Override(WellKnownMembers.XSharp_VO_Functions_VarGet, "MyVarGet")
        Compilation.Override(WellKnownMembers.XSharp_VO_Functions_VarPut, "MyVarPut")
        TestMacro(mc, e"{|| NIKOS}", <OBJECT>{}, "VarGet(NIKOS)", typeof(string))
        TestMacro(mc, e"{|| NIKOS := \"123\"}", <OBJECT>{}, "VarPut(NIKOS):123", typeof(string))

        XSharp.Runtime.MacroCompiler.Options:UndeclaredVariableResolution := VariableResolution.TreatAsField
        Compilation.Override(WellKnownMembers.XSharp_VO_Functions___FieldGet, "MyFieldGet")
        Compilation.Override(WellKnownMembers.XSharp_VO_Functions___FieldSet, "MyFieldSet")
        Compilation.Override(WellKnownMembers.XSharp_VO_Functions___FieldGetWa, "MyFieldGetWa")
        Compilation.Override(WellKnownMembers.XSharp_VO_Functions___FieldSetWa, "MyFieldSetWa")
        TestMacro(mc, e"{|| NIKOS}", <OBJECT>{}, "FieldGet(NIKOS)", typeof(string))
        TestMacro(mc, e"{|| NIKOS := \"123\"}", <OBJECT>{}, "FieldSet(NIKOS):123", typeof(string))
        TestMacro(mc, e"{|| _FIELD->NIKOS}", <OBJECT>{}, "FieldGet(NIKOS)", typeof(string))
        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS}", <OBJECT>{}, "FieldGet(BASE,NIKOS)", typeof(string))
        TestMacro(mc, e"{|| BASE->NIKOS}", <OBJECT>{}, "FieldGet(BASE,NIKOS)", typeof(string))
        TestMacro(mc, e"{|| _FIELD->NIKOS := \"123\"}", <OBJECT>{}, "FieldSet(NIKOS):123", typeof(string))
        TestMacro(mc, e"{|| _FIELD->BASE->NIKOS := \"123\"}", <OBJECT>{}, "FieldSet(BASE,NIKOS):123", typeof(string))
        TestMacro(mc, e"{|| BASE->NIKOS := \"123\"}", <OBJECT>{}, "FieldSet(BASE,NIKOS):123", typeof(string))

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
        catch e as XSharp.MacroCompiler.CompilationError
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
        internal static Options := MacroOptions{} as MacroOptions
        internal compiler := Compilation.Create<object,RuntimeCodeblockDelegate>(options) as Compilation<object,RuntimeCodeblockDelegate>

	    public method Compile (cMacro as string, lOldStyle as logic, Module as System.Reflection.Module, lIsBlock ref logic) as ICodeBlock
		    lIsBlock := cMacro:StartsWith("{|")
            var m := compiler:Compile(cMacro)
            if m:Diagnostic != null
                throw m:Diagnostic
            endif
    	    return RuntimeCodeblock{m:Macro,m:ParamCount}

	    public method Compile (cMacro as string) as ICodeBlock
            var m := compiler:Compile(cMacro)
            if m:Diagnostic != null
                throw m:Diagnostic
            endif
    	    return RuntimeCodeblock{m:Macro,m:ParamCount}
    end class

end namespace
