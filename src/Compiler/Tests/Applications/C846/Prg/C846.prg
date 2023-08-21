// 846. Usage of System.Drawing.Size in Macro expressions give errors
// https://github.com/X-Sharp/XSharpPublic/issues/1003
FUNCTION Start() AS VOID
// make sure assemblies are loaded
? System.Drawing.Size.Empty
? System.Windows.Forms.AnchorStyles.Left

// all the following run with no errors
? MCompile("System.Int32.MaxValue")
xAssert(MExec(MCompile("System.Int32.MaxValue")) == System.Int32.MaxValue)
? MCompile("System.Windows.Forms.AnchorStyles.Left")
xAssert( MExec(MCompile("System.Windows.Forms.AnchorStyles.Left")) == System.Windows.Forms.AnchorStyles.Left)
? MCompile("System.Windows.Forms.Form{}")

LOCAL oArrayList AS System.Collections.ArrayList

oArrayList := MExec(MCompile("System.Collections.ArrayList{}"))
oArrayList := MExec(MCompile("System.Collections.ArrayList{123}"))
xAssert(oArrayList:Capacity == 123)
? MExec(MCompile("System.Collections.BitArray{1,FALSE}"))

// those throw a System.NullReferenceException
LOCAL oSize AS System.Drawing.Size
oSize := MExec(MCompile("System.Drawing.Size.Empty"))
xAssert(oSize:Width == 0 .and. oSize:Height == 0)
oSize := MExec(MCompile("System.Drawing.Size{1,2}"))
xAssert(oSize:Width == 1 .and. oSize:Height == 2)

PROC xAssert(l AS LOGIC) 
IF .NOT. l
	THROW Exception{"Incorrect result in line " + System.Diagnostics.StackTrace{TRUE}:GetFrame(1):GetFileLineNumber():ToString()}
END IF
? "Assertion passed"   
RETURN 	

/*
System.NullReferenceException
Object reference not set to an instance of an object.

Callstack : 
static CompilationError XSharp.MacroCompiler.Compilation.Error(XSharp.MacroCompiler.Syntax.Token t, XSharp.MacroCompiler.ErrorCode e, System.Object[] args)() 
static CompilationError XSharp.MacroCompiler.Binder.ConversionError(XSharp.MacroCompiler.Syntax.Expr expr, XSharp.MacroCompiler.TypeSymbol type)() 
static System.Void XSharp.MacroCompiler.Binder.Convert(XSharp.MacroCompiler.Syntax.Expr e, XSharp.MacroCompiler.TypeSymbol type, XSharp.MacroCompiler.ConversionSymbol conv)() 
System.Void XSharp.MacroCompiler.Binder.Convert(XSharp.MacroCompiler.Syntax.Expr e, XSharp.MacroCompiler.TypeSymbol type)() 
XSharp.MacroCompiler.Syntax.ExprResultStmt.Node XSharp.MacroCompiler.Syntax.ExprResultStmt.Bind(XSharp.MacroCompiler.Binder b)() 
XSharp.MacroCompiler.Binder`2[XSharp.__Usual,XSharp.MacroCompiler.UsualMacro.MacroCodeblockDelegate].System.Void XSharp.MacroCompiler.Binder.BindStmt(System.Object node)() 
Node XSharp.MacroCompiler.Syntax.Codeblock.Bind(XSharp.MacroCompiler.Binder b)() 
System.Void XSharp.MacroCompiler.Binder.Bind(System.Object node)() 
Object XSharp.MacroCompiler.Binder`2[XSharp.__Usual,XSharp.MacroCompiler.UsualMacro.MacroCodeblockDelegate].Bind(System.Object macro)() 
XSharp.MacroCompiler.Compilation`2[XSharp.__Usual,XSharp.MacroCompiler.UsualMacro.MacroCodeblockDelegate].CompilationResult XSharp.MacroCompiler.Compilation`2[XSharp.__Usual,XSharp.MacroCompiler.UsualMacro.MacroCodeblockDelegate].Compile(System.String source)() 
_Codeblock XSharp.Runtime.MacroCompiler.CompileCodeblock(System.String macro, System.Boolean lAllowSingleQuotes, System.Reflection.Module module)() 
static _Codeblock XSharp.RT.Functions.MCompile(System.String cString, System.Boolean lAllowSingleQuotes)() 
static _Codeblock XSharp.RT.Functions.MCompile(System.String cString)() 
static System.Void xRuntime.Exe.Functions.Start()()   :  C:\VIDE\Projects\Project3\Applications\xRuntime\Prg\xRntime.prg  :  15
*/
