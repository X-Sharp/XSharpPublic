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
        TestGlobals.tsi := teststruct{1}
        TestGlobals.tci := testclass{1}

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

        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsFieldOrMemvar
        LOCAL obj := Error{"Some description"} AS OBJECT
        __LocalPut("obj", obj)
        TestMacro(mc, "obj.Description", Args(), "Some description",typeof(STRING))
        IF __LocalsUpdated()
            obj := __LocalGet("obj")
        ENDIF
        __LocalsClear()

        mc:Options:UndeclaredVariableResolution := VariableResolution.TreatAsField
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGet, "MyFieldGet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSet, "MyFieldSet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldGetWa, "MyFieldGetWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___FieldSetWa, "MyFoxFieldSetWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___pushWorkarea, "MyPushWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___popWorkarea, "MyPopWa")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___MemVarGet, "MyMemVarGet")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___MemVarPut, "MyFoxMemVarPut")
        Compilation.Override(WellKnownMembers.XSharp_RT_Functions___VarPut, "MyFoxVarPut")

        // Check .NULL.
        //        TestMacro(mc, e"{|a| a > DBNull.Value }", Args(8), False, typeof(LOGIC))
        //        TestMacro(mc, e"{|a| a = .NULL. }", Args(8), False, typeof(LOGIC))
        //        TestMacro(mc, e"{|a| a < .NULL. }", Args(8), False, typeof(LOGIC))
        //        TestMacro(mc, e"{|a| a + .NULL. }", Args(8), DbNull.Value, typeof(USUAL))
        //        TestMacro(mc, e"{|a| a * .NULL. }", Args(8), DbNull.Value, typeof(USUAL))

        // USUAL defaults to FALSE for FoxPro
        TestMacro(mc, e"{|a| a := default(usual) }", Args(8), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|a| a := NIL }", Args(8), FALSE, typeof(LOGIC))

        // FoxPro Null_Date
        TestMacro(mc, e"{|| { / / } }", Args(), NULL_DATE, typeof(DATE))
        TestMacro(mc, e"{|| {//} }", Args(), NULL_DATE, typeof(DATE))
        TestMacro(mc, e"{|| {..} }", Args(), NULL_DATE, typeof(DATE))
        TestMacro(mc, e"{|| {--} }", Args(), NULL_DATE, typeof(DATE))
        TestMacro(mc, e"{|| 1<2.and.3<.4 }", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|| 1<2. and .3<.4 }", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|| 1<2 .and. 3<.4 }", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|| 3<2.or.3<2 }", Args(), FALSE, typeof(LOGIC))
        TestMacro(mc, e"{|| 3<2. or .3<2 }", Args(), TRUE, typeof(LOGIC))
        TestMacro(mc, e"{|| 3<2 .or. 3<2 }", Args(), FALSE, typeof(LOGIC))


        // FoxPro dot access
        TestMacro(mc, e"{|| testclass{}.NString((byte)1) }", Args(), "child", typeof(STRING))
        TestMacro(mc, e"{|a| a := testclass{}, a.prop }", Args(), 0, typeof(INT))
        TestMacro(mc, e"{|a| a := testclass{}, a.prop := 111 }", Args(), 111, typeof(INT))
        /*
            RvdH VO does not allow TO ACCESS GLOBAL and DEFINE values IN the macro compiler
            And FoxPro does not even have GLOBAL and DEFINE
        TestMacro(mc, e"{|| tsi.v1 }", Args(), 1, typeof(INT)) // FAIL because access to globals not allowed, see commit c9107824
        TestMacro(mc, e"{|| tci.v1 := 10, tci.v1++, tci.v1 }", Args(), 11, typeof(INT)) // FAIL because access to globals not allowed, see commit c9107824
        */
        TestMacro(mc, e"{|| TestGlobals.tsi.v1 }", Args(), 1, typeof(INT))
        TestMacro(mc, e"{|| TestGlobals.tci.v1 := 10, TestGlobals.tci.v1++, TestGlobals.tci.v1 }", Args(), 11, typeof(INT))
        TestMacro(mc, e"{|| DEVS.NIKOS}", Args(), "FieldGet(DEVS,NIKOS)", typeof(STRING))
        TestMacro(mc, e"{|| DEVS.NIKOS := \"123\"}", Args(), "FoxFieldSet(DEVS,NIKOS):123", typeof(STRING))
        TestMacro(mc, e"{|| M.NAME}", Args(), "MemVarGet(NAME)", typeof(STRING))
        TestMacro(mc, e"{|| M.NAME := \"Nikos\"}", Args(), "FoxMemVarPut(NAME):Nikos", typeof(STRING))
        TestMacro(mc, e"{|| @@M.NAME}", Args(), "FieldGet(M,NAME)", typeof(STRING))
        //TestMacro(mc, e"{|| @@M.NAME := \"Nikos\"}", Args(), "FoxFieldSet(M,NAME):Nikos", typeof(STRING))
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
        TestMacro(mc, "Qout(.NULL.),.NULL."      ,Args(), DBNull.Value, typeof(DBNull))
       Console.WriteLine("Total pass: {0}/{1}", TotalSuccess, TotalTests)
        RETURN

END NAMESPACE

