//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser
USING LanguageService.CodeAnalysis.Text
USING XSharp.Parser
USING XP := LanguageService.CodeAnalysis.XSharp.SyntaxParser.XSharpParser
NAMESPACE XSharpModel
PARTIAL CLASS SourceWalker IMPLEMENTS IDisposable , VsParser.IErrorListener

METHOD AntlrParse(cSource AS STRING, stream OUT ITokenStream) AS XSharpParserRuleContext
    stream := NULL
    WriteOutputMessage("-->> AntlrParse() "+SELF:SourcePath+" Start "+DateTime.Now.ToString())
    XSharp.Parser.VsParser.Parse(cSource, SELF:SourcePath, (XSharpParseOptions) SELF:ParseOptions,SELF,OUT stream, OUT VAR tree, OUT VAR includeFiles)
    self:AddIncludes(includeFiles)
    WriteOutputMessage("<<-- AntlrParse() "+SELF:SourcePath+" End "+DateTime.Now.ToString())
    RETURN tree

METHOD ConvertParseTree(tree as XSharpParserRuleContext) AS VOID
    WriteOutputMessage("-->> ConvertParseTree() "+SELF:SourcePath+" Start "+DateTime.Now.ToString())
    VAR listener := XSharpAntlrWalker{SELF}
    var walker := LanguageService.SyntaxTree.Tree.ParseTreeWalker{}
    walker:Walk(listener, tree)

    WriteOutputMessage("<<-- ConvertParseTree() "+SELF:SourcePath+" End "+DateTime.Now.ToString())
    RETURN

METHOD ProcessNamespace(nsCtx as XP.Namespace_Context) AS VOID

    RETURN
METHOD ProcessType(typeCtx as XP.ITypeContext) AS VOID
    // Possible types
    // XP.VostructContext
    // XP.VounionContext
    // XP.Interface_Context
    // XP.Class_Context
    // XP.Structure_Context
    // XP.Delegate_Context
    // XP.XppclassContext
    // XP.FoxclassContext
    // XP.IPartialPropertyContext
    RETURN
METHOD ProcessMember(memberCtx as XP.IMemberContext) AS VOID
    // XP.Possible members
    // XP.FuncprocContext
    // XP.VodllContext
    // XP.FoxdllContext
    // XP.VoglobalContext
    // XP.MethodContext
    // XP.VodefineContext
    // XP.Delegate_Context
    // XP.Event_Context
    // XP.EventAccessorContext
    // XP.PropertyContext
    // XP.PropertyLineAccessorContext
    // XP.PropertyAccessorContext
    // XP.ConstructorContext
    // XP.DestructorContext
    // XP.Operator_Context
    // XP.LocalfuncprocContext
    // XP.XppdeclarepropertyContext
    // XP.XppmethodContext
    // XP.XppinlineMethodContext
    // XP.FoxmethodContext
    // XP.IGlobalEntityContext
    // XP.IMemberWithBodyContext
    // XP.IXPPMemberContext
    // XP.IMethodContext

    RETURN
METHOD ProcessEnum(enumCtx as XP.Enum_Context) AS VOID

    RETURN
METHOD ProcessFoxSource(foxSrcCtx as XP.FoxsourceContext) AS VOID

    RETURN
METHOD ProcessSource(srcCtx as XP.SourceContext) AS VOID

    RETURN

METHOD ProcessGlobalEntity(globalCtx as XP.IGlobalEntityContext) AS VOID
    RETURN

END CLASS


CLASS XSharpAntlrWalker INHERIT XSharpBaseListener
    PROTECTED Walker AS SourceWalker

CONSTRUCTOR(walker as SourceWalker)
    SUPER()
    SELF:Walker := walker
    SELF:Walker:ReportError("", LinePositionSpan{}, "","",null)
OVERRIDE METHOD EnterEveryRule(ctx AS ParserRuleContext) AS VOID
    SWITCH ctx
    CASE nsCtx AS XP.Namespace_Context
        SELF:Walker:ProcessNamespace(nsCtx)
    CASE globalCtx as XP.IGlobalEntityContext
        SELF:Walker:ProcessGlobalEntity(globalCtx)

    CASE typeCtx as XP.ITypeContext
        SELF:Walker:ProcessType(typeCtx)
    CASE memberCtx as XP.IMemberContext
        SELF:Walker:ProcessMember(memberCtx)
    CASE enumCtx as XP.Enum_Context
        SELF:Walker:ProcessEnum(enumCtx)
    CASE srcCtx as XP.SourceContext
        SELF:Walker:ProcessSource(srcCtx)
    CASE ctxfoxSrc as XP.FoxsourceContext
        SELF:Walker:ProcessFoxSource(ctxfoxSrc)
    END SWITCH
END CLASS
