﻿// VoTests.prg
// Created by    : nvk
// Creation Date : 2/13/2021 3:57:40 PM
// Created for   :
// WorkStation   : I7


USING System
USING System.Collections.Generic
USING System.Text
USING System.Linq
USING XSharp.Runtime
USING XSharp.MacroCompiler

GLOBAL MyGlobal := 42

BEGIN NAMESPACE MacroCompilerTest

    FUNCTION ResetOverrides() AS VOID
        // Reset overrides
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___VarGet)
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___VarPut)
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGet)
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSet)
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGetWa)
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSetWa)
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___pushWorkarea)
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___popWorkarea)
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___MemVarGet)
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___MemVarPut)

    FUNCTION TestByRefPriv() AS VOID
        PRIVATE x
        VAR mc := CreateMacroCompiler()
        // Access GLobal variables safely
        TestMacro(mc, e"{|| MyGlobal }", Args(), 42, typeof(INT))
        TestMacro(mc, e"{|| MyGlobal:= 84 }", Args(), 84, typeof(INT))
        TestMacro(mc, e"{|| MyGlobal }", Args(), 84, typeof(INT))

        TestMacro(mc, "{|| System.Drawing.Size{1,2}}", Args(),System.Drawing.Size{1,2}, typeof(System.Drawing.Size))
        TestMacro(mc, "{|| System.Drawing.Size.Empty}", Args(),System.Drawing.Size.Empty, typeof(System.Drawing.Size))
        x := "a"
        TestMacro(mc, "{||testfunc(x), x}", Args(), "b", typeof(STRING))
        x := "a"
        TestMacro(mc, "{||testfunc(ref x), x}", Args(), "b", typeof(STRING))
        x := "a"
        TestMacro(mc, "{||testfunc(@x), x}", Args(), "b", typeof(STRING))
        x := "a"
        TestMacro(mc, "{||testfunclate(x), x}", Args(), "a", typeof(STRING))
        x := "a"
        TestMacro(mc, "{||testfunclate(ref x), x}", Args(), "b", typeof(STRING))
        x := "a"
        TestMacro(mc, "{||testfunclate(@x), x}", Args(), "b", typeof(STRING))
        var c := TestRefCall{}
        x := "a"
        TestMacro(mc, "{|c|c:testmethod(x), x}", Args(c), "a", typeof(STRING)) // note: cannot auto-determine REF with late-bound calls!!!
        x := "a"
        TestMacro(mc, "{|c|c:testmethod(@x), x}", Args(c), "b", typeof(STRING))
        x := "a"
        TestMacro(mc, "{|c|c:testmethod(ref x), x}", Args(c), "b", typeof(STRING))
        x := "a"
        TestMacro(mc, "{|c|c:testmethodlate(@x), x}", Args(c), "b", typeof(STRING))
        x := "a"
        TestMacro(mc, "{|c|c:testmethodlate(ref x), x}", Args(c), "b", typeof(STRING))
        x := "a"
        TestMacro(mc, "{|c|Send(c,#testmethod,@x), x}", Args(c), "b", typeof(STRING))
        x := "a"
        TestMacro(mc, "{|c|Send(c,#testmethod,ref x), x}", Args(c), "b", typeof(STRING))
       x := "a"
        TestMacro(mc, "{|c|Send(c,#testmethodlate,@x), x}", Args(c), "b", typeof(STRING))
        x := "a"
        TestMacro(mc, "{|c|Send(c,#testmethodlate,ref x), x}", Args(c), "b", typeof(STRING))
        RETURN


    FUNCTION VoTests(mc AS XSharp.Runtime.MacroCompiler) AS VOID
        Console.WriteLine("Running VO tests ...")
        TestGlobals.tsi := teststruct{1}
        TestGlobals.tci := testclass{1}

        ResetOverrides()
        PUBLIC cExpr
        PUBLIC lExpr := TRUE
        m->cExpr := "Left('12345', 3)"
        m->lExpr := TRUE
        // test embedded macro expression
        ? TestMacro(mc, "&(cExpr)", Args(), "123", typeof(string) )
        ? TestMacro(mc, "IIF(lExpr, &(cExpr),'abc')", Args(), "123", typeof(string) )
        m->lExpr := FALSE
        ? TestMacro(mc, "IIF(lExpr,'abc' ,&(cExpr))", Args(), "123", typeof(string) )
        TestMacro(mc, e"{|| &('1+2') }", Args(), 3, TYPEOF(LONG))

        TestMacro(mc, "1 + (INT) +1", Args(), 2, TYPEOF(int))
        TestMacro(mc, "1 + (INT) -1", Args(), 0, TYPEOF(int))
        TestMacro(mc, "1 + (USUAL) +1", Args(), 2, TYPEOF(int))
        TestMacro(mc, "1 + (USUAL) -1", Args(), 0, TYPEOF(int))

        // Test dotted AND and OR in combination with numbers
        TestMacro(mc, e"{|| 1>2.and.3<4}", Args(), FALSE, TYPEOF(LOGIC))
        TestMacro(mc, e"{|| 1>2.or.3<4}", Args(), TRUE, TYPEOF(LOGIC))
        // Test new rules for REAL_CONST
        TestMacro(mc, e"{|| .1e10}", Args(), 1.0e9, TYPEOF(FLOAT))
        TestMacro(mc, e"{|| 1.e10}", Args(), 1.0e10, TYPEOF(FLOAT))
        TestMacro(mc, e"{|| 1.e-10}", Args(), 1.0e-10, TYPEOF(FLOAT))
        TestMacro(mc, e"{|| .1}", Args(), .1, TYPEOF(FLOAT))
        TestMacro(mc, e"{|| 4.}", Args(), 4.0, TYPEOF(FLOAT))
        TestMacro(mc, e"{|| 1.c}", Args(), "Unexpected 'c'", typeof(Exception),ErrorCode.Unexpected)

        TestMacroU(mc, e"{|a,b| S_EnforceType(a,b), a}", ArgsU(NIL,"N"), 0, typeof(INT))
        TestMacro(mc, e"{|a,b| S_EnforceType(a,b), a}", Args(NIL,"N"), 0, typeof(INT))
        TestMacro(mc, e"{|a,b| S_EnforceType(REF a,b), a}", Args(NIL,"N"), 0, typeof(INT))
        TestMacro(mc, e"{|a,b| S_EnforceTypeC(REF a,b), a}", Args(NIL,"N"), 0, typeof(INT))

        //TestParse(mc, e"{|a,b| +a[++b] += 100, a[2]}", "{|a, b|RETURN (((+a((++b)))+='100'), a('2'))}")
        TestMacro(mc, e"{|v|(v := upper(v), left(v,3))}", Args("ABCDE"), "ABC", typeof(STRING))
        TestMacro(mc, e"{|l, v|iif (l, (v := upper(v), left(v,3)), (v := lower(v), left(v,4))) }", Args(TRUE, "ABCDE"), "ABC", typeof(STRING))
        TestMacro(mc, e"{|l, v|iif (l, (v := upper(v), left(v,3)), (v := lower(v), left(v,4))) }", Args(FALSE, "ABCDE"), "abcd", typeof(STRING))
        TestMacro(mc, e"{|a,b| asdgfafd(123) /*aaa*/ }", Args(), NULL, NULL,ErrorCode.NotAMethod)

        // bug 1186
        TestMacro(mc, e"{|| ctest.fld}", Args(), "Variable does not exist", typeof(XSharp.Error))
        TestMacro(mc, "MyNS.StaticClass.Test", Args(), NULL, NULL, ErrorCode.NotAnExpression)

        mc:Options:UndeclaredVariableResolution := VariableResolution.Error
        TestMacro(mc, e"{|a,b| testtest__() }", Args(1,2,3), NULL, NULL, ErrorCode.IdentifierNotFound)
        TestMacro(mc, e"{|a,b,c| a[b,c] }", Args({{42,43,44},{45,46,47}},1,1) ,42, typeof(LONG))
        // The next 2 items are for Github issue #238
        TestMacro(mc, e"{|a| a == 1 }", Args(1), TRUE, typeof(LOGIC),)
        TestMacro(mc, e"{|a| a == 1 }", Args(TRUE), TRUE, typeof(LOGIC),)
        // The following items handle the late bound call of a method / property with the a name that matches a property inside the Usual type
        TestMacro(mc, e"{|a| a:Item()}", Args(TestWithItem{}), 42,typeof(LONG))
        TestMacro(mc, e"{|a| a:Nested:Item()}", Args(TestWithItem{}), 42,typeof(LONG))
        TestMacro(mc, e"{|a| a:Item}", Args(TestWithItem2{}), 42,typeof(LONG))
        TestMacro(mc, e"{|a| a:Nested:Item}", Args(TestWithItem2{}), 42,typeof(LONG))
        TestMacro(mc, e"{|o| eval({|a| eval({|q|q*q},a)+1 },o) }", Args(5), 26, typeof(INT))
        TestMacro(mc, e"{|o| eval( iif(o, {||42},{||-42})) }", Args(TRUE), 42, typeof(INT))
        TestMacro(mc, e"{|o| eval( iif(o, {||42},{||-42})) }", Args(FALSE), -42, typeof(INT))
        TestMacro(mc, e"{|e| if( e = 1, 'true','false' ) ", Args(1), "true", typeof(String) )

        mc:Options:FoxParenArrayAccess := true
        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsFieldOrMemvar
        M->Foo := __FoxArray{10}
        M->Foo := __FoxAssign(M->Foo ,42)

        TestMacro(mc, e"{|a,b| asdgfafd := a, asdgfafd(2) }", Args(M->Foo, 1), 42, typeof(INT))
        TestMacro(mc, e"{|aq| aq(1)}", Args(M->Foo, 1), 42, typeof(int))
        TestMacro(mc, e"{|a| a(3) += a(2) + a(1), a(3)}", Args(M->Foo), 126, typeof(int))
        M->FoxArrayTest := __FoxArray{2,3}
        M->FoxArrayTest[1,1] := 10
        M->FoxArrayTest[1,2] := 20
        M->FoxArrayTest[1,3] := 30
        M->FoxArrayTest[2,1] := 40
        M->FoxArrayTest[2,2] := 50
        M->FoxArrayTest[2,3] := 60
        // When 'FoxArrayTest' is an aray and we use 1 or 2 indices
        // then access the arrar
        TestMacro(mc, e"{|| FoxArrayTest(1,1)}", Args(), 10, typeof(int))
        // Note that FoxPro allows to access the 2 dimensional array with 1 dimension as well

        TestMacro(mc, e"{|| FoxArrayTest(1)}", Args(), 10, typeof(int))
        TestMacro(mc, e"{|| FoxArrayTest(6)}", Args(), 60, typeof(int))
        TestMacro(mc, e"{|| FoxArrayTest(1,2)}", Args(), 20, typeof(int))
        TestMacro(mc, e"{|| FoxArrayTest(1,3)}", Args(), 30, typeof(int))
        TestMacro(mc, e"{|| FoxArrayTest(2,1)}", Args(), 40, typeof(int))
        TestMacro(mc, e"{|| FoxArrayTest(2,2)}", Args(), 50, typeof(int))
        TestMacro(mc, e"{|| FoxArrayTest(2,3)}", Args(), 60, typeof(int))
        //
        // with 3 dimensions the macro compiler call the function
        TestMacro(mc, e"{|| FoxArrayTest(10,20,30)}", Args(), 102, typeof(int))
        // The next line should call the function because '2' and '3' are
        // invalid array indexes, should call function
        TestMacro(mc, e"{|| FoxArrayTest('2','3')}", Args(), 42, typeof(int))
        //
        // when the value of the public is not an array then the runtime
        // calls the function.  this returns 42 +  the sum of the indices
        M->FoxArrayTest := "testme"
        TestMacro(mc, e"{|| FoxArrayTest(10,10)}", Args(), 62, typeof(int))
        TestMacro(mc, e"{|| FoxArrayTest(20,20)}", Args(), 82, typeof(int))

        mc:Options:FoxParenArrayAccess := false

        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsFieldOrMemvar
        TestMacro(mc, e"{|| eval({||true}) }", Args(), true, typeof(logic))
        TestMacro(mc, e"{|o| eval({|a|a},o) }", Args(TRUE), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|o| eval({|a|a},o) }", Args(false), false, typeof(logic))
        TestMacro(mc, e"{|| x := 42, eval({||x}) }", Args(), 42, typeof(int))
        TestMacro(mc, e"{|| x := true, eval({||x := 42}), x }", Args(), 42, typeof(int))
        TestMacro(mc, e"{|o| eval({|q|q*q},o) + eval({|a| a+1 },o) }", Args(5), 31, typeof(int))

        mc:Options:UndeclaredVariableResolution := VariableResolution.GenerateLocal
        TestMacro(mc, e"{|a| a() }", Args((@@Func<INT>){ => 1234}), 1234, typeof(INT))
        TestMacro(mc, "#HELLo", Args(), #hello, typeof(SYMBOL))
        TestMacro(mc, "#HELLo + #World", Args(), #hello + #world, typeof(STRING))
        TestMacro(mc, e"#HELLo + \"world\"", Args(), #hello + "world", typeof(STRING))
        TestMacro(mc, e"\"Hello\" + #world", Args(), "Hello" + #world, typeof(STRING))
        // Literals of type string with bracketed delimiters
        TestMacro(mc, "[Hello] + [world]", Args(), "Helloworld", typeof(STRING))
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
        TestMacro(mc, "0000.00.00 == NULL_DATE", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, "null", Args(), NULL, typeof(OBJECT))
        TestMacro(mc, "null_object", Args(), NULL_OBJECT, NULL)
        TestMacro(mc, "null_string", Args(), NULL_STRING, NULL)
        TestMacro(mc, "null_psz = psz{IntPtr.Zero}", Args(), TRUE, typeof(LOGIC))
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
        TestMacro(mc, e"{|a| 0 <> 0}", Args(8), FALSE, typeof(LOGIC))
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


        // RvdH VO does not allow to access GLOBAL and DEFINE values in the macro compiler
        /*
        TestMacro(mc, e"{|| tsi:v1 }", Args(), 1, typeof(INT))
        TestMacro(mc, e"{|| ++tsi:v1 }", Args(), 2, typeof(INT))
//        TestMacro(mc, e"{|| tsi:v1 := 10, tsi:v1 }", Args(), 10, typeof(int)) // FAIL because tsi is boxed by value for IVarPut()
        TestMacro(mc, e"{|| ++tsi:prop }", Args(), 2, typeof(INT))
        TestMacro(mc, e"{|| tci:v1 }", Args(), 1, typeof(INT))
        TestMacro(mc, e"{|| ++tci:v1 }", Args(), 2, typeof(INT))
        TestMacro(mc, e"{|| ++tci:prop }", Args(), 2, typeof(INT))
        TestMacro(mc, e"{|| tci:v1 := 10, tci:v1++, tci:v1 }", Args(), 11, typeof(INT))
        TestMacro(mc, e"-tsi", Args(), NULL, NULL, ErrorCode.UnaryOperationNotFound)
        TestMacro(mc, e"tsi+1", Args(), NULL, NULL, ErrorCode.BinaryOperationNotFound)
        TestMacro(mc, e"tsi[2]", Args(), NULL, NULL, ErrorCode.NoConversion)
        TestMacro(mc, e"{|| tci:GetHashCode() }", Args(), tci:GetHashCode(), typeof(INT))
        TestMacro(mc, e"{|| tsi:GetHashCode() }", Args(), tsi:GetHashCode(), typeof(INT))
        TestMacro(mc, e"{|| tci:ToString() }", Args(), "testclass", typeof(STRING))
        TestMacro(mc, e"{|| tsi:ToString() }", Args(), "teststruct", typeof(STRING))
        TestMacro(mc, e"{|| tsi:FString() }", Args(), "1", typeof(STRING))
        TestMacro(mc, e"{|| tci:FString('fff') }", Args(), "fff1", typeof(STRING))
        TestMacro(mc, e"{|| tsi:FString('fff') }", Args(), "fff1", typeof(STRING))
        TestMacro(mc, e"{|| ((testbase)tci):FString() }", Args(), "1", typeof(STRING))
        TestMacro(mc, e"{|a| tci:&a := 1 }", Args("v1"), 1, typeof(INT))
        TestMacro(mc, e"{|a| tci:&a }", Args("v1"), 1, typeof(INT))
        TestMacro(mc, e"{|a| tci:&(a) }", Args("v1"), 1, typeof(INT))
        TestMacro(mc, e"{|a| tci:(&a) }", Args("v1"), 1, typeof(INT))
        */

        TestMacro(mc, e"{|| TestGlobals.tsa:length }", Args(), 5, typeof(INT))
        TestMacro(mc, e"{|| TestGlobals.tsa[1] }", Args(), 11, typeof(INT))
        TestMacro(mc, e"{|| TestGlobals.tsa[5] }", Args(), 55, typeof(INT))
        TestMacro(mc, e"{|| TestGlobals.tsa2[1,1] }", Args(), 11, typeof(INT))
        TestMacro(mc, e"{|| TestGlobals.tsa2[1,2] }", Args(), 22, typeof(INT))
        TestMacro(mc, e"{|| TestGlobals.tsi:v1 }", Args(), 1, typeof(INT))
        TestMacro(mc, e"{|| ++TestGlobals.tsi:v1 }", Args(), 2, typeof(INT))
//        TestMacro(mc, e"{|| TestGlobals.tsi:v1 := 10, TestGlobals.tsi:v1 }", Args(), 10, typeof(int)) // FAIL because tsi is boxed by value for IVarPut()
        TestMacro(mc, e"{|| ++TestGlobals.tsi:prop }", Args(), 2, typeof(INT))
        TestMacro(mc, e"{|| TestGlobals.tci:v1 }", Args(), 1, typeof(INT))
        TestMacro(mc, e"{|| ++TestGlobals.tci:v1 }", Args(), 2, typeof(INT))
        TestMacro(mc, e"{|| ++TestGlobals.tci:prop }", Args(), 2, typeof(INT))
        TestMacro(mc, e"{|| TestGlobals.tci:v1 := 10, TestGlobals.tci:v1++, TestGlobals.tci:v1 }", Args(), 11, typeof(INT))
        TestMacro(mc, e"-TestGlobals.tsi", Args(), NULL, NULL, ErrorCode.UnaryOperationNotFound)
        TestMacro(mc, e"TestGlobals.tsi+1", Args(), NULL, NULL, ErrorCode.BinaryOperationNotFound)
        TestMacro(mc, e"TestGlobals.tsi[2]", Args(), NULL, NULL, ErrorCode.IndexerNotFound)
        TestMacro(mc, e"{|| TestGlobals.tci:GetHashCode() }", Args(), TestGlobals.tci:GetHashCode(), typeof(INT))
        TestMacro(mc, e"{|| TestGlobals.tsi:GetHashCode() }", Args(), TestGlobals.tsi:GetHashCode(), typeof(INT))
        TestMacro(mc, e"{|| TestGlobals.tci:ToString() }", Args(), "testclass", typeof(STRING))
        TestMacro(mc, e"{|| TestGlobals.tsi:ToString() }", Args(), "teststruct", typeof(STRING))
        TestMacro(mc, e"{|| TestGlobals.tsi:FString() }", Args(), "1", typeof(STRING))
        TestMacro(mc, e"{|| TestGlobals.tci:FString('fff') }", Args(), "fff1", typeof(STRING))
        TestMacro(mc, e"{|| TestGlobals.tsi:FString('fff') }", Args(), "fff1", typeof(STRING))
        TestMacro(mc, e"{|| ((testbase)TestGlobals.tci):FString() }", Args(), "1", typeof(STRING))
        TestMacro(mc, e"{|a| TestGlobals.tci:&a := 1 }", Args("v1"), 1, typeof(INT))
        TestMacro(mc, e"{|a| TestGlobals.tci:&a }", Args("v1"), 1, typeof(INT))
        TestMacro(mc, e"{|a| TestGlobals.tci:&(a) }", Args("v1"), 1, typeof(INT))
        TestMacro(mc, e"{|a| TestGlobals.tci:(&a) }", Args("v1"), 1, typeof(INT))

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
        TestMacro(mc, e"{|a,b| int is ValueType }", Args(), true, typeof(logic)) // 'int' is treated as ID, generates local
        TestMacro(mc, e"{|a,b| int }", Args(), NULL, NULL) // 'int' is treated as ID, generates local
        TestMacro(mc, e"{|a,b| U(int) }", Args(), NULL, NULL) // 'int' is treated as ID, generates local
        TestMacro(mc, e"{|a,b| (int)a }", Args(123U), 123, typeof(int))
///        TestMacro(mc, "U", Args(), null, null, ErrorCode.NotAnExpression) // It is treated as field/memvar
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
        TestMacro(mc, e"'AA' <> U('A')", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"'AA' != 'A'", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"'AA' <> 'A'", Args(), FALSE, typeof(LOGIC))
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
        TestMacro(mc, e"{|a,b| 1[2]}", Args(), NULL, NULL, ErrorCode.UnExpected)
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
        TestMacro(mc, e"{|a| XSharp.RT.Functions.ALen(a) }", Args({"1235454","54376","12","53"},NIL), 4, typeof(DWORD))
        //TestMacro(mc, e"{|a| (testclass)a }",Args(tci), tci, typeof(testclass))
        TestMacro(mc, e"{|a| ((int)a):GetHashCode() }", Args(8), 8, typeof(INT))
        TestMacro(mc, e"{|a| a:GetHashCode() }", Args(8), 8, typeof(INT))
        TestMacro(mc, e"{|a| a:GetHashCode() }", Args(tci), tci:GetHashCode(), typeof(INT))
        TestMacro(mc, e"{|a| a:GetHashCode() }", Args(tsi), tsi:GetHashCode(), typeof(INT))
        TestMacro(mc, e"{|a| ((int)a):ToString() }", Args(8), "8", typeof(STRING))
        TestMacro(mc, e"{|a| a:ToString() }", Args(8), "8", typeof(STRING))
        TestMacro(mc, e"{|a| a:ToString() }", Args(tci), "testclass", typeof(STRING))
        TestMacro(mc, e"{|a| a:ToString() }", Args(tsi), "teststruct", typeof(STRING))
        TestMacro(mc, e"{|a| a:UString() }", Args(tci), "1", typeof(STRING))
        TestMacro(mc, e"{|a| a:UString() }", Args(tsi), "1", typeof(STRING))
        TestMacro(mc, e"{|| testbase{}:FString() }", Args(), "base", typeof(STRING))
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
        TestMacro(mc, "SubStr3('Test', 1 , SLen('abc') + 1)", Args(), "Test", typeof(STRING)) // should raise warning
        TestMacro(mc, "SubStr3('Test', 1 , Len('abc') - 1)", Args(), "Te", typeof(STRING)) // should raise warning
        TestMacro(mc, "TestInt32(TestDWord(1))", Args(), 1, typeof(int)) // should raise warning
        TestMacro(mc, "TestDWord(TestInt32(1))", Args(), 1, typeof(dword)) // should raise warning
        TestMacro(mc, e"{|a,b| a := \"abcdef\", a:(&b)() }", Args(8,"ToUpperInvariant"), "ABCDEF", typeof(STRING))
        TestMacro(mc, e"{|a,b| a := \"abcdef\", (a:&b)() }", Args(8,"ToUpperInvariant"), "ABCDEF", typeof(STRING))
        TestMacro(mc, e"{|a,b| a := \"abcdef\", a:&(b)() }", Args(8,"ToUpperInvariant"), NULL, NULL, ErrorCode.Expected)
        TestMacro(mc, e"{|a,b| a := \"abcdef\", (a:&testRet(b))() }", Args(8,"ToUpperInvariant"), "ABCDEF", typeof(STRING))
        TestMacro(mc, e"{|a,b| a := \"abcdef\", a:&testRet(b)() }", Args(8,"ToUpperInvariant"), NULL, NULL, ErrorCode.ArgumentsNotMatch)
        TestMacro(mc, e"{|a,b| a := \"abcdef\", a:(&b[1])() }", Args(8,{"ToUpperInvariant"}), "ABCDEF", typeof(STRING))
        TestMacro(mc, e"{|a,b| a := \"abcdef\", (a:&b[1])() }", Args(8,{"ToUpperInvariant"}), "ABCDEF", typeof(STRING))
        //TestMacro(mc, e"{|| Error}", Args(), 321, typeof(int))
        TestMacro(mc, e"{|| Error{\"TEST\"}:Message}", Args(), "TEST", typeof(string))
        TestMacro(mc, e"{|| Error := 123}", Args(), 123, typeof(int))
        //TestMacro(mc, e"{|| ErrorLevel}", Args(), 1, typeof(int))
        TestMacro(mc, e"{|| ErrorLevel()}", Args(), 0, typeof(dword))
        TestMacro(mc, e"{|| ErrString(0)}", Args(), "", typeof(string))
        TestMacro(mc, e"{|| ErrString.V}", Args(), 333, typeof(int))
        TestMacro(mc, e"System.Collections.ArrayList{}:GetType():FullName", Args(), "System.Collections.ArrayList", typeof(string))
        TestMacro(mc, e"TestNS.Nested.TestClass{}:GetType():FullName", Args(), "TestNS.Nested.TestClass", typeof(string))
        TestMacro(mc, e"TestNS2.TestClass{}:GetType():FullName", Args(), "TestNS2.TestClass", typeof(string))
        TestMacro(mc, e"{|a| a == 1 }", Args((INT)1) ,true, typeof(LOGIC))
        TestMacro(mc, e"{|a| a == 1 }", Args((DWORD)1) ,true, typeof(LOGIC))
        TestMacro(mc, e"{|a| a == 1 }", Args((BYTE)1) ,true, typeof(LOGIC))
        TestMacro(mc, e"{|a| a == 1 }", Args((SBYTE)1) ,true, typeof(LOGIC))
        TestMacro(mc, e"{|a| a == 1 }", Args((WORD)1) ,true, typeof(LOGIC))
        TestMacro(mc, e"{|a| a == 1 }", Args((INT64)1) ,true, typeof(LOGIC))
        TestMacro(mc, e"{|a| a == 1 }", Args((UINT64)1) ,true, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a == b }", Args((INT64)1,(INT)1) ,true, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a == b }", Args((INT)1,(UINT64)1) ,true, typeof(LOGIC))
        TestMacro(mc, e"{|a,b| a == b }", Args((INT64)1,(UINT64)1) ,true, typeof(LOGIC))
        TestMacro(mc, "{|| date():gettype():FullName }", Args(), "XSharp.__Date", typeof(string))
        TestMacro(mc, "{|| datetime():gettype():FullName }", Args(), "System.DateTime", typeof(string))
        TestMacro(mc, "{|| alen(array()) }", Args(), 0, typeof(dword))
        TestMacro(mc, "{ || $123 } ", Args(), 123m, typeof(currency))
        TestMacro(mc, "{ || $123.456 } ", Args(), 123.456m, typeof(currency))
        TestMacro(mc, "{ || $.5 } ", Args(), .5m, typeof(currency))
        TestMacro(mc, "{ || $123_456 } ", Args(), 123456m, typeof(currency))
        TestMacro(mc, e"{|a| TestByRef(a) }", Args("123"), "123", typeof(string))
        TestMacro(mc, "{ |x| (x)-1 } ",Args(2),1,typeof(int))
        TestMacro(mc, "{ || (int)(-5.1) } ",Args(),-5,typeof(int))
        TestMacro(mc, "{ || TestInNotNil(10) } ",Args(),true,typeof(logic))
        TestMacro(mc, "{ || TestRefNotZero(10) } ",Args(),true,typeof(logic))
        TestMacro(mc, '"#include ""XSharpDefs.xh"" "', Args(), "#include ""XSharpDefs.xh"" ", typeof(string))
        TestMacro(mc, "'#include ''XSharpDefs.xh'' '", Args(), '#include ''XSharpDefs.xh'' ', typeof(string))
        TestMacro(mc, "'#include [XSharpDefs.xh] '", Args(), '#include [XSharpDefs.xh] ', typeof(string))

        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___MemVarGet, "MyMemVarGet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___MemVarPut, "MyMemVarPut")
        TestMacro(mc, e"{|| M->NAME}", Args(), "MemVarGet(NAME)", typeof(STRING))
        TestMacro(mc, e"{|| M->NAME := \"Nikos\"}", Args(), "MemVarPut(NAME):Nikos", typeof(STRING))
        TestMacro(mc, e"{|| MEMVAR->NAME}", Args(), "MemVarGet(NAME)", typeof(STRING))
        TestMacro(mc, e"{|| MEMVAR->NAME := \"Nikos\"}", Args(), "MemVarPut(NAME):Nikos", typeof(STRING))
        TestMacro(mc, e"{|| _MEMVAR->NAME}", Args(), "MemVarGet(NAME)", typeof(STRING))
        TestMacro(mc, e"{|| _MEMVAR->NAME := \"Nikos\"}", Args(), "MemVarPut(NAME):Nikos", typeof(STRING))
        TestMacro(mc, e"{|| M.NAME}", Args(), "MemVarGet(NAME)", typeof(STRING))
        TestMacro(mc, e"{|| M.NAME := \"Nikos\"}", Args(), "MemVarPut(NAME):Nikos", typeof(STRING))

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
        TestMacro(mc, e"Test1->(MyDbDo(\"F1\"))+MyDbDo(\"F2\")", Args(), "Test1->Do(F1)Do(F2)", typeof(string))
        TestMacro(mc, e"Test1->MyDbDo(\"F1\")+MyDbDo(\"F2\")", Args(), "Test1->Do(F1)Test1->Do(F2)", typeof(string))
        TestMacro(mc, e"{|| M->NAME}", Args(), "MemVarGet(NAME)", typeof(STRING))
        TestMacro(mc, e"{|| M->NAME := \"Nikos\"}", Args(), "MemVarPut(NAME):Nikos", typeof(STRING))
        TestMacro(mc, e"{|| @@M->NAME}", Args(), "FieldGet(M,NAME)", typeof(STRING))
        TestMacro(mc, e"{|| @@M->NAME := \"Nikos\"}", Args(), "FieldSet(M,NAME):Nikos", typeof(STRING))
        TestMacro(mc, e"{|| BASE->NIKOS == BASE->NIKOS}", Args(), true, typeof(logic))
        TestMacro(mc, e"{|| (BASE)->(NIKOS) == (BASE)->(NIKOS)}", Args(), true, typeof(logic))
        TestMacro(mc, e"{|| (BASE)->(NIKOS)}", Args(), "BASE->FieldGet(NIKOS)", typeof(string))
        TestMacro(mc, e"{|a| a := (BASE)->(NIKOS)}", Args(), "BASE->FieldGet(NIKOS)", typeof(string))
        TestMacro(mc, "{|o| IIF(o == NIL, _FIELD->SOME, _FIELD->SOME := o) }", Args(), "FieldGet(SOME)", typeof(string))
        TestMacro(mc, "{|o| IIF(o == NIL, _FIELD->SOME, _FIELD->SOME := o) }", Args(123), "FieldSet(SOME):123", typeof(string))

        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsFieldOrMemvar
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___VarGet, "MyVarGet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___VarPut, "MyVarPut")
        TestMacro(mc, e"{|| NIKOS}", Args(), "VarGet(NIKOS)", typeof(STRING))
        TestMacro(mc, e"{|| NIKOS := \"123\"}", Args(), "VarPut(NIKOS):123", typeof(STRING))
        TestMacro(mc, e"{|a,b| asdgfafd(123) }", Args(), NULL, NULL, ErrorCode.NotAMethod)
        TestMacro(mc, e"{|| CODE+SET}", Args(), "VarGet(CODE)VarGet(SET)", typeof(STRING))
        TestMacro(mc, e"{|| LONG}", Args(), "VarGet(LONG)", typeof(STRING))

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

        // Tests by Robert
        Testmacro(mc, e"{| | 0h0123456789abcde } ", Args(), NULL, NULL, ErrorCode.BinaryIncorrectLength )
        Testmacro(mc, e"{| | 0h } ", Args(), NULL, NULL, ErrorCode.BinaryIncorrectLength )
        Testmacro(mc, e"{| | 0h0123456789abcdef } ", Args(), 0h0123456789abcdef, typeof(BINARY) )
        Testmacro(mc, e"{| | 0h0123_4567_89ab_cdef } ", Args(), 0h0123456789abcdef, typeof(BINARY) )
        Testmacro(mc, e"{| | (STRING) 0h41 } ", Args(), "A", typeof(STRING) )
        Testmacro(mc, e"{| | (BINARY) \"A\" } ", Args(), 0h41, typeof(BINARY) )
        Testmacro(mc, e"{|str, x| PadL(str, x)}",Args("abc",2),"ab",typeof(STRING))
        TestMacro(mc, e"{|o|o:Checked}", Args(CheckBox{}), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a,b|AltD(), a+b}", Args(1,2), 3, typeof(LONG))
        var tempGetInst := System.Runtime.InteropServices.Marshal.GetHINSTANCE(typeof(XSharp.Core.Functions):Module)
        TestMacro(mc, e"{||_GetInst()}", Args(), tempGetInst, typeof(IntPtr))

        // Dynamic assembly load test (note: XSharp.VO needs to be compiled before this for it to work! This is ensured by the solution build order)
        //TestMacro(mc, "{ || FloatNext(0) } ", Args(), null, null, ErrorCode.NotAMethod)
        //System.Reflection.Assembly.Load("XSharp.VO")
        //TestMacro(mc, "{ || FloatNext(0) } ", Args(), 0.0, typeof(float))
        //typed and untyped parameters
        TestMacro(mc,"{|a,b| a + b }", Args(1,2), 3, typeof(INT))
        TestMacro(mc,"{|a as int,b as int| a + b }", Args(1,2), 3, typeof(INT))
        RuntimeState.MacroCompilerErrorHandler := SubstituteErrorWithNil
        TestMacro(mc, "{|| Left('abc,10) }", Args(),NULL, typeof(OBJECT))
        RuntimeState.MacroCompilerErrorHandler := SubstituteErrorWithNilMacro
        TestMacro(mc, "{|| 0h }", Args(),NULL, typeof(OBJECT))
        TestMacro(mc, "{|| 0h1 }", Args(),NULL, typeof(OBJECT))
        RuntimeState.MacroCompilerErrorHandler := NULL
        TestMacro(mc, "{|| Left('abc,10) }", Args(),typeof(Exception),NULL, ErrorCode.UnterminatedString)

        TestMacro(mc, "{|| 10/6 }", Args(), FLOAT(10)/6, typeof(FLOAT))


        TestMacro(mc,"{|oTest| oTest:Testx(5)}", Args( Test{}), 5, typeof(INT))
        TestMacro(mc,"{|oTest| oTest:Testx('abc')}", Args( Test{}), "abc", typeof(STRING))
        TestMacro(mc,"{|oTest| ((ITest)oTest):Testx(5)}", Args( Test{}), 5, typeof(INT))
        TestMacro(mc,"{|oTest| ((ITest)oTest):Testx('abc')}", Args( Test{}), "abc", typeof(STRING))


        Console.WriteLine("Total pass: {0}/{1}", TotalSuccess, TotalTests)
        RETURN

        FUNCTION SubstituteErrorWithNil (cmacro as string, oEx as Exception) as ICodeblock
            Console.WriteLine()
            Console.WriteLine(i"Intercepted error in macro ""{cmacro}"" :")
            Console.WriteLine(oEx:Message)
            //Console.ReadLine()
            return {|| NIL}

        FUNCTION SubstituteErrorWithNilMacro (cmacro as string, oEx as Exception) as ICodeblock
            //Console.ReadLine()
            Console.WriteLine()
            Console.WriteLine(i"Intercepted error in macro ""{cmacro}"" :")
            Console.WriteLine(oEx:Message)
            // Recursive, dangerous
            return MCompile("NIL")


END NAMESPACE






