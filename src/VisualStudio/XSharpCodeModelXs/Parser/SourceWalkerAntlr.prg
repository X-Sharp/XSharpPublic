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

PRIVATE _antlrConversionFailed AS LOGIC

METHOD AntlrParse(cSource AS STRING, stream OUT ITokenStream) AS XSharpParserRuleContext
    stream := NULL
    WriteOutputMessage("-->> AntlrParse() "+SELF:SourcePath+" Start "+DateTime.Now.ToString())
    XSharp.Parser.VsParser.Parse(cSource, SELF:SourcePath, (XSharpParseOptions) SELF:ParseOptions,SELF,OUT stream, OUT VAR tree, OUT VAR includeFiles)
    self:AddIncludes(includeFiles)
    WriteOutputMessage("<<-- AntlrParse() "+SELF:SourcePath+" End "+DateTime.Now.ToString())
    RETURN tree

METHOD ConvertParseTree(tree as XSharpParserRuleContext) AS LOGIC
    WriteOutputMessage("-->> ConvertParseTree() "+SELF:SourcePath+" Start "+DateTime.Now.ToString())
    TRY
        SELF:_antlrConversionFailed := FALSE
        VAR listener := XSharpAntlrWalker{SELF}
        var walker := LanguageService.SyntaxTree.Tree.ParseTreeWalker{}
        walker:Walk(listener, tree)
        WriteOutputMessage("<<-- ConvertParseTree() "+SELF:SourcePath+" End "+DateTime.Now.ToString())
        RETURN !SELF:_antlrConversionFailed
    CATCH e AS Exception
        WriteOutputMessage("ConvertParseTree() Exception: "+e:Message)
        XSettings.Exception(e)
        SELF:_antlrConversionFailed := TRUE
        RETURN FALSE
    END TRY

PRIVATE METHOD ExtractModifiers(ctx AS XP.IEntityContext) AS Modifiers
    LOCAL mods := Modifiers.None AS Modifiers
    IF ctx == NULL
        RETURN mods
    ENDIF
    
    TRY
        // Check for visibility modifiers
        VAR modifierCtx := SELF:GetModifierContext(ctx)
        IF modifierCtx != NULL
            VAR text := modifierCtx:GetText():ToLower()
            IF text:Contains("public")
                mods |= Modifiers.Public
            ELSEIF text:Contains("private")
                mods |= Modifiers.Private
            ELSEIF text:Contains("protected")
                IF text:Contains("internal")
                    mods |= Modifiers.ProtectedInternal
                ELSE
                    mods |= Modifiers.Protected
                ENDIF
            ELSEIF text:Contains("internal")
                mods |= Modifiers.Internal
            ENDIF
            
            // Check for other modifiers
            IF text:Contains("static")
                mods |= Modifiers.Static
            ENDIF
            IF text:Contains("abstract")
                mods |= Modifiers.Abstract
            ENDIF
            IF text:Contains("sealed")
                mods |= Modifiers.Sealed
            ENDIF
            IF text:Contains("virtual")
                mods |= Modifiers.Virtual
            ENDIF
            IF text:Contains("override")
                mods |= Modifiers.Override
            ENDIF
            IF text:Contains("partial")
                mods |= Modifiers.Partial
            ENDIF
            IF text:Contains("async")
                mods |= Modifiers.Async
            ENDIF
            IF text:Contains("const")
                mods |= Modifiers.Const
            ENDIF
            IF text:Contains("extern") .OR. text:Contains("external")
                mods |= Modifiers.External
            ENDIF
        ENDIF
    CATCH e AS Exception
        WriteOutputMessage("ExtractModifiers() Exception: "+e:Message)
    END TRY
    
    RETURN mods

PRIVATE METHOD GetModifierContext(ctx AS XP.IEntityContext) AS XSharpParserRuleContext
    // Try to get the modifiers context from the entity
    // Different entity types have different paths to modifiers
    IF ctx IS XP.Class_Context VAR classCtx
        RETURN classCtx:Modifiers
    ELSEIF ctx IS XP.Interface_Context VAR ifaceCtx
        RETURN ifaceCtx:Modifiers
    ELSEIF ctx IS XP.Structure_Context VAR structCtx
        RETURN structCtx:Modifiers
    ELSEIF ctx IS XP.Enum_Context VAR enumCtx
        RETURN enumCtx:Modifiers
    ELSEIF ctx IS XP.MethodContext VAR methodCtx
        RETURN methodCtx:Mods
    ELSEIF ctx IS XP.PropertyContext VAR propCtx
        RETURN propCtx:Modifiers
    ELSEIF ctx IS XP.FuncprocContext VAR funcCtx
        RETURN funcCtx:FuncProcModifiers
    ENDIF
    RETURN NULL

PRIVATE METHOD GetEntityName(ctx AS XP.IEntityContext) AS STRING
    IF ctx == NULL
        RETURN ""
    ENDIF
    TRY
        RETURN ctx:Name
    CATCH
        RETURN ""
    END TRY

PRIVATE METHOD GetSourceRange(ctx AS ParserRuleContext, range OUT TextRange, interval OUT TextInterval) AS VOID
    IF ctx != NULL .AND. ctx:Start != NULL .AND. ctx:Stop != NULL
        range := TextRange{ctx:Start, ctx:Stop}
        interval := TextInterval{ctx:Start, ctx:Stop}
    ELSE
        range := TextRange{}
        interval := TextInterval{}
    ENDIF

PRIVATE METHOD DetermineKind(ctx AS XP.IEntityContext) AS Kind
    IF ctx IS XP.Class_Context
        RETURN Kind.Class
    ELSEIF ctx IS XP.Interface_Context
        RETURN Kind.Interface
    ELSEIF ctx IS XP.Structure_Context
        RETURN Kind.Structure
    ELSEIF ctx IS XP.Enum_Context
        RETURN Kind.Enum
    ELSEIF ctx IS XP.Delegate_Context
        RETURN Kind.Delegate
    ELSEIF ctx IS XP.MethodContext
        RETURN Kind.Method
    ELSEIF ctx IS XP.PropertyContext
        RETURN Kind.Property
    ELSEIF ctx IS XP.ConstructorContext
        RETURN Kind.Constructor
    ELSEIF ctx IS XP.DestructorContext
        RETURN Kind.Destructor
    ELSEIF ctx IS XP.OperatorContext
        RETURN Kind.Operator
    ELSEIF ctx IS XP.EventContext
        RETURN Kind.Event
    ELSEIF ctx IS XP.FuncprocContext
        RETURN Kind.Function
    ELSEIF ctx IS XP.Namespace_Context
        RETURN Kind.Namespace
    ENDIF
    RETURN Kind.Unknown

METHOD ProcessNamespace(nsCtx as XP.Namespace_Context) AS VOID
    TRY
        VAR nsName := nsCtx:Name
        SELF:GetSourceRange(nsCtx, OUT VAR range, OUT VAR interval)
        VAR ns := XSourceNamespaceSymbol{nsName, range, interval}
        ns:File := SELF:File
        SELF:_entities:Add(ns)
    CATCH e AS Exception
        WriteOutputMessage("ProcessNamespace() Exception: "+e:Message)
        SELF:_antlrConversionFailed := TRUE
    END TRY
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
    TRY
        VAR typeName := SELF:GetEntityName(typeCtx)
        VAR kind := SELF:DetermineKind(typeCtx)
        VAR mods := SELF:ExtractModifiers(typeCtx)
        SELF:GetSourceRange(typeCtx, OUT VAR range, OUT VAR interval)
        
        VAR xType := XSourceTypeSymbol{typeName, kind, mods, range, interval, SELF:File, NULL}
        SELF:_entities:Add(xType)
    CATCH e AS Exception
        WriteOutputMessage("ProcessType() Exception: "+e:Message)
        SELF:_antlrConversionFailed := TRUE
    END TRY
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
    TRY
        VAR memberName := SELF:GetEntityName(memberCtx)
        VAR kind := SELF:DetermineKind(memberCtx)
        VAR mods := SELF:ExtractModifiers(memberCtx)
        SELF:GetSourceRange(memberCtx, OUT VAR range, OUT VAR interval)
        
        VAR returnType := SELF:GetReturnType(memberCtx)
        VAR xMember := XSourceMemberSymbol{memberName, kind, mods, range, interval, returnType, NULL, mods:HasFlag(Modifiers.Static)}
        SELF:_entities:Add(xMember)
    CATCH e AS Exception
        WriteOutputMessage("ProcessMember() Exception: "+e:Message)
        SELF:_antlrConversionFailed := TRUE
    END TRY
    RETURN

PRIVATE METHOD GetReturnType(memberCtx AS XP.IMemberContext) AS STRING
    TRY
        IF memberCtx:ReturnType != NULL
            RETURN memberCtx:ReturnType:GetText()
        ENDIF
    CATCH
        NOP
    END TRY
    RETURN ""

METHOD ProcessBlock(blockCtx as XP.StatementBlockContext) AS VOID
    // Process StatementBlock to generate blocks for the editor
    // Only when blockCtx.Parent is not an entity
    if blockCtx:Parent is XP.IEntityContext
        RETURN
    endif
    
    TRY
        SELF:GetSourceRange(blockCtx, OUT VAR range, OUT VAR interval)
        VAR block := XSourceBlock{range, interval}
        SELF:_blocks:Add(block)
    CATCH e AS Exception
        WriteOutputMessage("ProcessBlock() Exception: "+e:Message)
        SELF:_antlrConversionFailed := TRUE
    END TRY
    RETURN

METHOD ProcessEnum(enumCtx as XP.Enum_Context) AS VOID
    TRY
        VAR enumName := enumCtx:Name
        VAR mods := SELF:ExtractModifiers(enumCtx)
        SELF:GetSourceRange(enumCtx, OUT VAR range, OUT VAR interval)
        
        VAR xEnum := XSourceTypeSymbol{enumName, Kind.Enum, mods, range, interval, SELF:File, NULL}
        SELF:_entities:Add(xEnum)
        
        // Process enum members if available
        VAR members := enumCtx:enummember()
        IF members != NULL
            FOREACH VAR memberCtx IN members
                VAR memberName := memberCtx:Name
                SELF:GetSourceRange((XP.IEntityContext)memberCtx, OUT VAR memRange, OUT VAR memInterval)
                VAR xMember := XSourceMemberSymbol{memberName, Kind.EnumMember, Modifiers.Public, memRange, memInterval, "", NULL, FALSE}
                SELF:_entities:Add(xMember)
            NEXT
        ENDIF
    CATCH e AS Exception
        WriteOutputMessage("ProcessEnum() Exception: "+e:Message)
        SELF:_antlrConversionFailed := TRUE
    END TRY
    RETURN

METHOD ProcessFoxSource(foxSrcCtx as XP.FoxsourceContext) AS VOID
    // Fox specific source processing
    RETURN

METHOD ProcessSource(srcCtx as XP.SourceContext) AS VOID
    // Process the source root context if needed
    RETURN

METHOD ProcessGlobalEntity(globalCtx as XP.IGlobalEntityContext) AS VOID
    // Global entities like using statements, extern, etc.
    TRY
        VAR name := SELF:GetEntityName(globalCtx)
        VAR kind := SELF:DetermineKind(globalCtx)
        VAR mods := SELF:ExtractModifiers(globalCtx)
        SELF:GetSourceRange(globalCtx, OUT VAR range, OUT VAR interval)
        
        VAR xEntity := XSourceMemberSymbol{name, kind, mods, range, interval, "", NULL, mods:HasFlag(Modifiers.Static)}
        SELF:_entities:Add(xEntity)
    CATCH e AS Exception
        WriteOutputMessage("ProcessGlobalEntity() Exception: "+e:Message)
        SELF:_antlrConversionFailed := TRUE
    END TRY
    RETURN

END CLASS


CLASS XSharpAntlrWalker INHERIT XSharpBaseListener
    PROTECTED Walker AS SourceWalker

CONSTRUCTOR(walker as SourceWalker)
    SUPER()
    SELF:Walker := walker
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
    CASE blockCtx as XP.StatementBlockContext
        SELF:Walker:ProcessBlock(blockCtx)
    END SWITCH
END CLASS
