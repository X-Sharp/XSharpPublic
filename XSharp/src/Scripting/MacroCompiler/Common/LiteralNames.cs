namespace XSharp.MacroCompiler
{
    internal static class VulcanTypeNames
    {
        internal const string CodeBlockType = "Codeblock";
        internal const string UsualType = "__Usual";
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
    }
    internal static class XSharpSpecialNames
    {
        internal const string ImpliedTypeName = "Xs$var";
        internal const string ScriptDummy = "XS$dummy";
        internal const string StaticLocalFieldNamePrefix = "Xs$StaticLocal$";
        internal const string StaticLocalInitFieldNameSuffix = "$init";
        internal const string StaticLocalLockFieldNameSuffix = "$lock";
        internal const string EventFieldNamePrefix = "Xs$Event$";

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

        internal const string VoPszList = "Xs$PszList";
        internal const string ClipperArgs = "Xs$Args";
        internal const string RecoverVarName = "Xs$Obj";
        internal const string ExVarName = "Xs$Exception";
        internal const string ReturnName = "Xs$Return";

        internal const string CoreFunctionsClass = VulcanFunctionNames.FunctionsClass;
        internal const string VOExeFunctionsClass = ".Exe.Functions";
        internal const string VODllFunctionsClass = ".Functions";
        internal const string ModuleName = "<Module>";

        internal const string ReturnType = "<XsRetT>";
        internal const string ArgumentType = "<XsArgT>";

    }
    internal static class VulcanFunctionNames
    {
        internal const string StringCompare = "__StringCompare";
        internal const string StringEquals = "__StringEquals";
        internal const string StringSubtract = "StringSubtract";
        internal const string InExactEquals = "__InexactEquals";
        internal const string InExactNotEquals = "__InexactNotEquals";
        internal const string VulcanToObject = "ToObject";
        internal const string VulcanIVarGet = "IVarGet";
        internal const string VulcanIVarPut = "IVarPut";
        internal const string VulcanSend = "__InternalSend";
        internal const string VulcanASend = "ASend";
        internal const string Eval = "Eval";
        internal const string FunctionsClass = "Functions";
        internal const string RunInitProcs = "RunInitProcs";
        internal const string GetElement = "__GetElement";
        internal const string SetElement = "__SetElement";
    }
    internal static class VulcanQualifiedFunctionNames
    {
        internal const string FieldGet = "global::VulcanRTFuncs.Functions.__FieldGet";
        internal const string FieldGetWa = "global::VulcanRTFuncs.Functions.__FieldGetWa";
        internal const string FieldSet = "global::VulcanRTFuncs.Functions.__FieldSet";
        internal const string FieldSetWa = "global::VulcanRTFuncs.Functions.__FieldSetWa";
        internal const string MemVarGet = "global::VulcanRTFuncs.Functions.__MemVarGet";
        internal const string MemVarPut = "global::VulcanRTFuncs.Functions.__MemVarPut";
        internal const string NullDate = "global::Vulcan.__VODate.NullDate";
        internal const string PszRelease = "global::Vulcan.Internal.CompilerServices.String2PszRelease";
        internal const string String2Psz = "global::Vulcan.Internal.CompilerServices.String2Psz";
        internal const string ArrayNew = "global::Vulcan.__Array.__ArrayNew";
        internal const string InStr = "global::VulcanRTFuncs.Functions.Instr";
        internal const string EnterSequence = "global::Vulcan.Internal.CompilerServices.EnterBeginSequence";
        internal const string ExitSequence = "global::Vulcan.Internal.CompilerServices.ExitBeginSequence";
        internal const string WrapException = "global::Vulcan.Error._WrapRawException";
        internal const string QQout = "global::VulcanRTFuncs.Functions.QQOut";
        internal const string Qout = "global::VulcanRTFuncs.Functions.QOut";
        internal const string Chr = "global::VulcanRTFuncs.Functions.Chr";
        internal const string PushWorkarea = "global::VulcanRTFuncs.Functions.__pushWorkarea";
        internal const string PopWorkarea = "global::VulcanRTFuncs.Functions.__popWorkarea";
        internal const string Evaluate = "global::VulcanRTFuncs.Functions.Evaluate";
        //internal const string Default = "global::VulcanRTFuncs.Functions.Default";
        //internal const string Accept = "global::VulcanRTFuncs.Functions._accept";
        //internal const string Wait = "global::VulcanRTFuncs.Functions._wait";
        //internal const string Quit = "global::VulcanRTFuncs.Functions._quit";
    }
    internal static class VulcanAssemblyNames
    {
        // please note that these MUST be lowercase !
        internal const string VulcanRT = "vulcanrt";
        internal const string VulcanRTFuncs = "vulcanrtfuncs";
    }
    internal static class VulcanNameSpaces
    {
        internal const string Vulcan = "Vulcan";
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
    }
    internal static class OperatorNames
    {
        internal const string Implicit = "op_Implicit";
        internal const string Explicit = "op_Explicit";
        internal const string Addition = "op_Addition";
        internal const string BitwiseAnd = "op_BitwiseAnd";
        internal const string BitwiseOr = "op_BitwiseOr";
        internal const string Decrement = "op_Decrement";
        internal const string Division = "op_Division";
        internal const string Equality = "op_Equality";
        internal const string ExclusiveOr = "op_ExclusiveOr";
        internal const string False = "op_False";
        internal const string GreaterThan = "op_GreaterThan";
        internal const string GreaterThanOrEqual = "op_GreaterThanOrEqual";
        internal const string Increment = "op_Increment";
        internal const string Inequality = "op_Inequality";
        internal const string LeftShift = "op_LeftShift";
        internal const string UnsignedLeftShift = "op_UnsignedLeftShift";
        internal const string LessThan = "op_LessThan";
        internal const string LessThanOrEqual = "op_LessThanOrEqual";
        internal const string LogicalNot = "op_LogicalNot";
        internal const string LogicalOr = "op_LogicalOr";
        internal const string LogicalAnd = "op_LogicalAnd";
        internal const string Modulus = "op_Modulus";
        internal const string Multiply = "op_Multiply";
        internal const string OnesComplement = "op_OnesComplement";
        internal const string RightShift = "op_RightShift";
        internal const string UnsignedRightShift = "op_UnsignedRightShift";
        internal const string Subtraction = "op_Subtraction";
        internal const string True = "op_True";
        internal const string UnaryNegation = "op_UnaryNegation";
        internal const string UnaryPlus = "op_UnaryPlus";
        internal const string Concatenate = "op_Concatenate";
        internal const string Exponent = "op_Exponent";
        internal const string IntegerDivision = "op_IntegerDivision";
        internal const string Like = "op_Like";
    }
}