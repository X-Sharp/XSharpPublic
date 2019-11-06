/*
   Copyright 2016-2017 XSharp B.V.

Licensed under the X# compiler source code License, Version 1.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.xsharp.info/licenses

Unless required by applicable law or agreed to in writing, software
Distributed under the License is distributed on an "as is" basis,
without warranties or conditions of any kind, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

namespace Microsoft.CodeAnalysis.CSharp
{

    internal static class VulcanSpecialNames
    {
        internal const string ClipperArgs = "$args";
    }
    internal static class OurTypeNames
    {
        internal const string CodeBlockType = "Codeblock";
        internal const string UsualType = "__Usual";
        internal const string ArrayBase = "__ArrayBase";
        internal const string PszType = "__Psz";
        internal const string VOStructAttribute = "VOStructAttribute";
        internal const string DefaultParameterAttribute = "DefaultParameterValueAttribute";
        internal const string ActualTypeAttribute = "ActualTypeAttribute";
        internal const string ClipperCallingConventionAttribute = "ClipperCallingConventionAttribute";
    }
    internal static class VulcanQualifiedTypeNames
    {
        internal const string Usual = "global::Vulcan.__Usual";
        internal const string Float = "global::Vulcan.__VOFloat";
        internal const string Date = "global::Vulcan.__VODate";
        internal const string Array = "global::Vulcan.__Array";
        internal const string Symbol = "global::Vulcan.__Symbol";
        internal const string Psz = "global::Vulcan.__Psz";
        internal const string Codeblock = "global::Vulcan.Codeblock";
        internal const string WinBool = "global::Vulcan.__WinBool";
        internal const string RuntimeState = "global::Vulcan.Runtime.State";
        internal const string ClipperCallingConvention = "global::Vulcan.Internal.ClipperCallingConventionAttribute";
        internal const string WrappedException = "global::Vulcan.Internal.VulcanWrappedException";
        internal const string DefaultParameter = "global::Vulcan.Internal.DefaultParameterValueAttribute";
        internal const string ActualType = "global::Vulcan.Internal.ActualTypeAttribute";
        internal const string Error = "global::Vulcan.Error";
        internal const string ClassLibrary = "global::Vulcan.Internal.VulcanClassLibraryAttribute";
        internal const string CompilerVersion = "global::Vulcan.Internal.VulcanCompilerVersion";
        internal const string IsInstance = "global::Vulcan.Internal.IsVOInstanceAttribute";
    }

    internal static class XSharpQualifiedTypeNames
    {
        internal const string Usual = "global::XSharp.__Usual";
        internal const string Float = "global::XSharp.__Float";
        internal const string Date = "global::XSharp.__Date";
        internal const string Array = "global::XSharp.__Array";
        internal const string Symbol = "global::XSharp.__Symbol";
        internal const string Psz = "global::XSharp.__Psz";
        internal const string Codeblock = "global::XSharp.Codeblock";
        internal const string WinBool = "global::XSharp.__WinBool";
        internal const string RuntimeState = "global::XSharp.RuntimeState";
        internal const string ClipperCallingConvention = "global::XSharp.Internal.ClipperCallingConventionAttribute";
        internal const string WrappedException = "global::XSharp.Internal.WrappedException";
        internal const string DefaultParameter = "global::XSharp.Internal.DefaultParameterValueAttribute";
        internal const string ActualType = "global::XSharp.Internal.ActualTypeAttribute";
        internal const string Error = "global::XSharp.Error";
        internal const string ClassLibrary = "global::XSharp.Internal.ClassLibraryAttribute";
        internal const string CompilerVersion = "global::XSharp.Internal.CompilerVersion";
        internal const string IsInstance = "global::XSharp.Internal.IsInstanceAttribute";
        internal const string XppAbstract = "global::XSharp.XPP.Abstract";
    }
    internal static class XSharpIntrinsicNames
    {
        // Note that these must all be specified in UPPER case
        internal const string PCount = "PCOUNT";
        internal const string ArgCount = "ARGCOUNT";
        internal const string ClipperArgs = "_ARGS";
        internal const string GetMParam = "_GETMPARAM";
        internal const string GetFParam = "_GETFPARAM";
        internal const string PCallNative = "PCALLNATIVE";
        internal const string CCallNative = "CCALLNATIVE";
        internal const string PCall = "PCALL";
        internal const string CCall = "CCALL";
        internal const string String2Psz = "STRING2PSZ";
        internal const string Cast2Psz = "CAST2PSZ";
        internal const string GetInst = "_GETINST";
        internal const string SLen = "SLEN";
        internal const string AltD = "ALTD";
        internal const string Chr = "CHR";
        internal const string _Chr = "_CHR";
        internal const string InitMethod = "INIT";
        internal const string InitClassMethod = "INITCLASS";
        internal const string AxitMethod = "AXIT";
        internal const string DoDefault = "DODEFAULT";
    }
    internal static class XSharpSpecialNames
    {
        internal const string ImpliedTypeName = "Xs$var";
        internal const string ScriptDummy = "XS$dummy";                      
        internal const string StaticLocalFieldNamePrefix = "Xs$StaticLocal$";
        internal const string StaticLocalInitFieldNameSuffix = "$init";
        internal const string StaticLocalLockFieldNameSuffix = "$lock";
        internal const string EventFieldNamePrefix = "Xs$Event$";
        internal const string AccessSuffix = "$Access";
        internal const string AssignSuffix = "$Assign";
        internal const string PropertySuffix = "$Method";

        internal const string DelegateNameSpace = "Xs$Delegates";
        internal const string PCallPrefix = "$PCall";
        internal const string PCallNativePrefix = "$PCallNative";
        internal const string AppInit = "$AppInit";
        internal const string AppExit = "$AppExit";
        internal const string InitProc1 = "$Init1";
        internal const string InitProc2 = "$Init2";
        internal const string InitProc3 = "$Init3";
        internal const string ExitProc = "$Exit";
        internal const string PCallProc = "$PCallGetDelegate";
        internal const string SymbolTable = "Xs$SymbolTable";
        internal const string PSZTable = "Xs$PSZLiteralsTable";
        internal const string ActionVariable = "Xs$a";

        internal const string VoPszList = "Xs$PszList";
        internal const string ClipperArgs = "Xs$Args";
        internal const string ClipperPCount = "Xs$PCount";
        internal const string RecoverVarName = "Xs$Obj";
        internal const string ExVarName = "Xs$Exception";
        internal const string ReturnName = "Xs$Return";
        internal const string ArrayName = "Xs$Array";
        internal const string WithVarName = "Xs$WithVar";
        internal const string PrivatesLevel = "Xs$PrivatesLevel";
        internal const string MemVarPrefix = "Xs$MemVar";
        internal const string FieldPrefix = "Xs$Field";

        internal const string FunctionsClass = "Functions";
        internal const string VOExeFunctionsClass = ".Exe.Functions";
        internal const string XSharpCoreFunctionsClass = "XSharp.Core.Functions";
        internal const string XSharpRDDFunctionsClass = "XSharp.RDD.Functions";
        internal const string XSharpRTFunctionsClass = "XSharp.RT.Functions";
        internal const string XSharpVOFunctionsClass = "XSharp.VO.Functions";
        internal const string XSharpXPPFunctionsClass = "XSharp.XPP.Functions";
        internal const string XSharpVFPFunctionsClass = "XSharp.VFP.Functions";
        internal const string VODllFunctionsClass = ".Functions";
        internal const string ModuleName = "<Module>";
        internal const string RTDialect = "Dialect";
        internal const string RTCompilerOptionVO11 = "CompilerOptionVO11";
        internal const string RTCompilerOptionVO13 = "CompilerOptionVO13";
        internal const string RTCompilerOptionFOvf = "CompilerOptionFOvf";
        internal const string RTCompilerOptionOvf = "CompilerOptionOvf";
        internal const string AddObject = "AddObject";
        internal const string GetProperty = "_GetProperty";
        internal const string SetProperty = "_SetProperty";
        internal const string InitProperties = "_InitProperties";


    }

    internal static class ReservedNames
    {
        // these are all expected in the VO Function type
        internal const string StringCompare = "__StringCompare";
        internal const string StringEquals = "__StringEquals";
        internal const string StringSubtract = "StringSubtract";
        internal const string StringAlloc = "StringAlloc";
        internal const string InExactEquals = "__InexactEquals";
        internal const string InExactNotEquals = "__InexactNotEquals";
        internal const string Evaluate = "Evaluate";
        internal const string ToObject = "ToObject";
        internal const string IVarGet = "IVarGet";
        internal const string IVarPut = "IVarPut";
        internal const string InternalSend = "__InternalSend";
        internal const string ASend = "ASend";
        internal const string Eval = "Eval";
        internal const string QQout = "QQOut";
        internal const string Qout = "QOut";
        internal const string GetElement = "__GetElement";
        internal const string SetElement = "__SetElement";
        // These are in the generated code
        internal const string RunInitProcs = "RunInitProcs";
        internal const string VarGet = "__VarGet";
        internal const string VarPut = "__VarPut";
        internal const string MemVarGet = "__MemVarGet";
        internal const string MemVarPut = "__MemVarPut";
        internal const string MemVarRelease = "__MemVarRelease";
        internal const string MemVarInit = "__MemVarInit";
        internal const string MemVarDecl = "__MemVarDecl";
        internal const string FieldGet = "__FieldGet";
        internal const string FieldPut = "__FieldSet";
        internal const string FieldGetWa = "__FieldGetWa";
        internal const string FieldPutWa = "__FieldSetWa";
        internal const string PushWorkarea ="__pushWorkarea";
        internal const string PopWorkarea = "__popWorkarea";
        internal const string String2Psz = "String2Psz";
        internal const string String2PszRelease = "String2PszRelease";
        internal const string StringArrayInit = "StringArrayInit";
        internal const string BeginSequenceEnter = "EnterBeginSequence";
        internal const string BeginSequenceExit = "ExitBeginSequence";
        internal const string TextSupport = "__TextSupport";
        internal const string TextOut = "__TextOut";
        internal const string TextMergeCheck = "SetTextMerge";
        internal const string Chr = "Chr";
        internal const string Instr = "Instr";
        internal const string AreaEval = "__AreaEval";
        internal const string ArrayNew = ".__ArrayNew";
        internal const string NullDate = ".NullDate";
        internal const string NIL = "._NIL";
        internal const string WrapRawExceptionVn = "._WrapRawException";
        internal const string WrapRawExceptionXs = ".WrapRawException";
    }
    internal static class VulcanQualifiedFunctionNames
    {
        internal const string VulcanRTFuncs = "global::VulcanRTFuncs.Functions.";
        internal const string CompilerServices = "global::Vulcan.Internal.CompilerServices.";
        internal const string FieldGet = VulcanRTFuncs + ReservedNames.FieldGet;
        internal const string FieldGetWa = VulcanRTFuncs + ReservedNames.FieldGetWa;
        internal const string FieldSet = VulcanRTFuncs + ReservedNames.FieldPut;
        internal const string FieldSetWa = VulcanRTFuncs + ReservedNames.FieldPutWa;
        internal const string NullDate = VulcanQualifiedTypeNames.Date +  ReservedNames.NullDate;
        internal const string UsualNIL = VulcanQualifiedTypeNames.Usual + ReservedNames.NIL;
        internal const string PszRelease = CompilerServices + ReservedNames.String2PszRelease;
        internal const string String2Psz = CompilerServices + ReservedNames.String2Psz;
        internal const string StringArrayInit = CompilerServices + ReservedNames.StringArrayInit;
        internal const string ArrayNew = VulcanQualifiedTypeNames.Array + ReservedNames.ArrayNew;
        internal const string InStr = VulcanRTFuncs + ReservedNames.Instr;
        internal const string EnterSequence = CompilerServices + ReservedNames.BeginSequenceEnter;
        internal const string ExitSequence = CompilerServices + ReservedNames.BeginSequenceExit;
        internal const string WrapException = VulcanQualifiedTypeNames.Error + ReservedNames.WrapRawExceptionVn;
        internal const string QQout = VulcanRTFuncs + ReservedNames.QQout;
        internal const string Qout = VulcanRTFuncs + ReservedNames.Qout;
        internal const string Chr = VulcanRTFuncs + ReservedNames.Chr;
        internal const string StringAlloc = VulcanRTFuncs + ReservedNames.StringAlloc;
        internal const string PushWorkarea = VulcanRTFuncs + ReservedNames.PushWorkarea;
        internal const string PopWorkarea = VulcanRTFuncs + ReservedNames.PopWorkarea;
        internal const string Evaluate = VulcanRTFuncs + ReservedNames.Evaluate;
        internal const string IVarGet = VulcanRTFuncs + ReservedNames.IVarGet;
        internal const string IVarPut = VulcanRTFuncs + ReservedNames.IVarPut;
    }

    internal static class XSharpQualifiedFunctionNames
    {
        internal const string GlobalRT = "global::" + XSharpSpecialNames.XSharpRTFunctionsClass + ".";
        internal const string GlobalCore = "global::" + XSharpSpecialNames.XSharpCoreFunctionsClass + ".";
        internal const string CompilerServices = "global::XSharp.Internal.CompilerServices.";
        // In core
        internal const string Chr = GlobalCore + ReservedNames.Chr;
        internal const string InStr = GlobalCore + ReservedNames.Instr;
        internal const string WrapException = XSharpQualifiedTypeNames.Error + ReservedNames.WrapRawExceptionXs;
        // In RT assembly
        internal const string FieldGet = GlobalRT + ReservedNames.FieldGet;
        internal const string FieldGetWa = GlobalRT + ReservedNames.FieldGetWa;
        internal const string FieldSet = GlobalRT + ReservedNames.FieldPut;
        internal const string FieldSetWa = GlobalRT + ReservedNames.FieldPutWa;
        internal const string AreaEval = GlobalRT + ReservedNames.AreaEval;
        internal const string MemVarGet = GlobalRT + ReservedNames.MemVarGet;
        internal const string MemVarPut = GlobalRT + ReservedNames.MemVarPut;
        internal const string MemVarInit = GlobalRT + ReservedNames.MemVarInit;
        internal const string MemVarRelease = GlobalRT + ReservedNames.MemVarRelease;
        internal const string MemVarDecl = GlobalRT + ReservedNames.MemVarDecl;
        internal const string NullDate = XSharpQualifiedTypeNames.Date + ReservedNames.NullDate;
        internal const string UsualNIL = XSharpQualifiedTypeNames.Usual + ReservedNames.NIL;
        internal const string PszRelease = CompilerServices + ReservedNames.String2PszRelease;
        internal const string String2Psz = CompilerServices + ReservedNames.String2Psz;
        internal const string StringArrayInit = CompilerServices + ReservedNames.StringArrayInit;
        internal const string ArrayNew = XSharpQualifiedTypeNames.Array + ReservedNames.ArrayNew;
        internal const string EnterSequence = CompilerServices + ReservedNames.BeginSequenceEnter;
        internal const string ExitSequence = CompilerServices + ReservedNames.BeginSequenceExit;
        internal const string QQout = GlobalRT + ReservedNames.QQout;
        internal const string Qout = GlobalRT + ReservedNames.Qout;
        internal const string StringAlloc = GlobalRT + ReservedNames.StringAlloc;
        internal const string PushWorkarea = GlobalRT + ReservedNames.PushWorkarea;
        internal const string PopWorkarea = GlobalRT + ReservedNames.PopWorkarea;
        internal const string Evaluate = GlobalRT + ReservedNames.Evaluate;
        internal const string IVarGet = GlobalRT + ReservedNames.IVarGet;
        internal const string IVarPut = GlobalRT + ReservedNames.IVarPut;
    }

    internal static class VulcanAssemblyNames
    {
        // please note that these MUST be lowercase !
        internal const string VulcanRT = "vulcanrt";
        internal const string VulcanRTFuncs = "vulcanrtfuncs";
    }
    internal static class XSharpAssemblyNames
    {
        // please note that these MUST be lowercase !
        internal const string XSharpCore = "xsharp.core";
        internal const string XSharpRT = "xsharp.rt";
        internal const string XSharpVO = "xsharp.vo";
        internal const string XSharpXPP = "xsharp.xpp";
        internal const string XSharpVFP = "xsharp.vfp";
        internal const string SdkDefines = "sdkdefines";
        internal const string VoGui = "voguiclasses";
        internal const string VoSystem = "vosystemclasses";
        internal const string VoRdd = "vorddclasses";
        internal const string VoSql = "vosqlclasses";
        internal const string VoConsole = "voconsoleclasses";
        internal const string VoWin32 = "vowin32apilibrary";
        internal const string VoInet = "vointernetclasses";
        internal const string VoReport = "voreportclasses";
    }

    internal static class OurNameSpaces
    {
        internal const string Vulcan = "Vulcan";
        internal const string XSharp = "XSharp";
    }

    internal static class SystemQualifiedNames
    {
        internal const string Cdecl = "global::System.Runtime.InteropServices.CallingConvention.Cdecl";
        internal const string ThisCall = "global::System.Runtime.InteropServices.CallingConvention.ThisCall";
        internal const string CompilerGenerated = "global::System.Runtime.CompilerServices.CompilerGenerated";
        internal const string CompilerGlobalScope = "global::System.Runtime.CompilerServices.CompilerGlobalScope";
        internal const string IntPtr = "global::System.IntPtr";
        internal const string DllImport = "global::System.Runtime.InteropServices.DllImportAttribute";
        internal const string CharSet = "global::System.Runtime.InteropServices.CharSet";
        internal const string WriteLine = "global::System.Console.WriteLine";
        internal const string Write = "global::System.Console.Write";
        internal const string Pow = "global::System.Math.Pow";
        internal const string DebuggerBreak = "global::System.Diagnostics.Debugger.Break";
        internal const string Debugger = "global::System.Diagnostics.Debugger";
        internal const string GetHInstance = "global::System.Runtime.InteropServices.Marshal.GetHINSTANCE";
        internal const string Exception = "global::System.Exception";
        internal const string StructLayout = "global::System.Runtime.InteropServices.StructLayout";
        internal const string LayoutExplicit = "global::System.Runtime.InteropServices.LayoutKind.Explicit";
        internal const string LayoutSequential = "global::System.Runtime.InteropServices.LayoutKind.Sequential";
        internal const string FieldOffset = "global::System.Runtime.InteropServices.FieldOffset";
        internal const string GetDelegate = "global::System.Runtime.InteropServices.Marshal.GetDelegateForFunctionPointer";
        internal const string GcCollect = "global::System.Gc.Collect";
        internal const string GcWait = "global::System.Gc.WaitForPendingFinalizers";
        internal const string Void1 = "System.Void";
        internal const string Void2 = "global::System.Void";
        internal const string CollectionsGeneric = "global::System.Collections.Generic";
        internal const string NonSerialized = "global::System.NonSerializedAttribute";
    }
}


