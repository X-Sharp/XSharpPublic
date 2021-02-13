// FoxTests.prg
// Created by    : nvk
// Creation Date : 2/13/2021 3:55:51 PM
// Created for   : 
// WorkStation   : I7


USING System
USING System.Collections.Generic
USING System.Text
USING System.Linq
USING XSharp.Runtime
USING XSharp.MacroCompiler

BEGIN NAMESPACE MacroCompilerTest

    FUNCTION FoxTests(mc AS XSharp.Runtime.MacroCompiler) AS VOID
        Console.WriteLine("Running FOX tests ...")
        XSharp.RuntimeState.Dialect := XSharp.XSharpDialect.FoxPro

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
        TestMacro(mc, e"{|a| a := NIL }", Args(8), FALSE, typeof(LOGIC))

        // FoxPro dot access
        TestMacro(mc, e"{|| testclass{}.NString((byte)1) }", Args(), "child", typeof(STRING))
        TestMacro(mc, e"{|a| a := testclass{}, a.prop }", Args(), 0, typeof(INT))
        TestMacro(mc, e"{|a| a := testclass{}, a.prop := 111 }", Args(), 111, typeof(INT))
        //TestMacro(mc, e"{|| tsi.v1 }", Args(), 1, typeof(INT)) // FAIL because access to globals not allowed, see commit c9107824
        //TestMacro(mc, e"{|| tci.v1 := 10, tci.v1++, tci.v1 }", Args(), 11, typeof(INT)) // FAIL because access to globals not allowed, see commit c9107824
        TestMacro(mc, e"{|| TestGlobals.tsi.v1 }", Args(), 1, typeof(INT))
        TestMacro(mc, e"{|| TestGlobals.tci.v1 := 10, TestGlobals.tci.v1++, TestGlobals.tci.v1 }", Args(), 11, typeof(INT))
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

        XSharp.RuntimeState.Dialect := XSharp.XSharpDialect.VO
        Console.WriteLine("Total pass: {0}/{1}", TotalSuccess, TotalTests)
        RETURN

END NAMESPACE

