//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//
USING System
USING System.IO
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Threading.Tasks
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser
USING STATIC XSharp.Parser.VsParser
USING LanguageService.CodeAnalysis.Text
USING XSharp.Parser
USING XSharp.Settings

BEGIN NAMESPACE XSharpModel
CLASS SourceWalker IMPLEMENTS IDisposable , VsParser.IErrorListener
#region fields
    PRIVATE _errors     AS IList<XError>
    PRIVATE _file       AS XFile
    PRIVATE _entities   AS IList<XSourceEntity>
    PRIVATE _blocks     AS IList<XSourceBlock>
    PRIVATE _locals     AS IList<XSourceVariableSymbol>
    PRIVATE _includeFiles as IList<string>
    PRIVATE _source     as string

#endregion
#region Properties
    PROPERTY HasParseErrors AS LOGIC AUTO

    PRIVATE PROPERTY ProjectNode as IXSharpProject GET SELF:_file?:Project?:ProjectNode
    PRIVATE PROPERTY ParseOptions AS XSharpParseOptions GET SELF:ProjectNode?:ParseOptions

    PROPERTY SourcePath AS STRING AUTO  // Save it because calculation the XAML source path is a bit expensive
    PROPERTY IncludeFiles AS IList<string>      GET _includeFiles
    PROPERTY EntityList AS IList<XSourceEntity> GET _entities
    PROPERTY BlockList  AS IList<XSourceBlock>  GET _blocks
    PROPERTY File AS XFile GET _file
    PROPERTY SaveToDisk AS LOGIC AUTO
#endregion

    CONSTRUCTOR(file AS XFile, lSaveResults := TRUE AS LOGIC )
        SUPER()
        SELF:SaveToDisk := lSaveResults
        IF (file != NULL)
            SELF:_file := file
            SELF:SourcePath := SELF:_file:SourcePath
            SELF:_errors   := List<XError>{}
            SELF:_entities := List<XSourceEntity>{}
            SELF:_blocks   := List<XSourceBlock>{}
            SELF:_locals   := List<XSourceVariableSymbol>{}

        ENDIF

    VIRTUAL METHOD Dispose() AS VOID
        SELF:Dispose(TRUE)

    PROTECTED VIRTUAL METHOD Dispose(disposing AS LOGIC) AS VOID
        IF disposing
            //
            SELF:_errors := NULL
            SELF:_file := NULL
            SELF:_blocks   := NULL
            SELF:_locals   := NULL
        ENDIF
        IF SELF:SaveToDisk
            SELF:_entities := NULL
        ENDIF



    METHOD Lex(cSource AS STRING) AS BufferedTokenStream
        LOCAL lOk := FALSE AS LOGIC
        _source := cSource
        WriteOutputMessage("-->> Lex() "+SourcePath+" ("+cSource:Length:ToString()+")")
        SELF:_errors := List<XError>{}
        LOCAL stream := NULL AS ITokenStream
        TRY
            XSharp.Parser.VsParser.Lex(cSource, SELF:SourcePath, SELF:ParseOptions, SELF, OUT stream, OUT VAR includeFiles)
            SELF:AddIncludes(includeFiles)
        CATCH e AS Exception
            WriteOutputMessage("Lex() Failed:")
            WriteOutputMessage(SELF:SourcePath)
            XSettings.Exception(e, __FUNCTION__)
        END TRY
        WriteOutputMessage("<<-- Lex() "+SELF:SourcePath)
        RETURN (BufferedTokenStream) stream

    METHOD AddIncludes(includeFiles as IList<string>) AS VOID
        self:_includeFiles := null
        if includeFiles != null
            self:_includeFiles := includeFiles
            SELF:_file:IncludeFiles:Clear()
            foreach var fileName in includeFiles
                SELF:_file:IncludeFiles:Add(XInclude{fileName})
            next
        endif

    METHOD AntlrParse(cSource AS STRING, stream OUT ITokenStream) AS XSharpParserRuleContext
        stream := NULL
        // WriteOutputMessage("-->> AntlrParse() "+SELF:SourcePath+" Start "+DateTime.Now.ToString())
        // XSharp.Parser.VsParser.Parse(cSource, SELF:SourcePath,SELF:ParseOptions,SELF,OUT stream, OUT VAR tree, OUT VAR includeFiles)
        // AddIncludes(includeFiles)
        // WriteOutputMessage("<<-- AntlrParse() "+SELF:SourcePath+" End "+DateTime.Now.ToString())
        RETURN NULL // tree

    METHOD ParseLocals(source AS STRING, xmember AS XSourceMemberSymbol) AS List<XSourceVariableSymbol>
        // This is JUST the source of the method. The locations in the variables need to be adjusted

        VAR owner := xmember:Parent
        VAR startLine := xmember:Range:StartLine
        VAR startIndex := xmember:Interval:Start+1
        VAR sb := StringBuilder{}
        _source := source
        local nLines := 0 as INT
        // get the includes and defines from the start of the file
        // until the entity that we are inspecting
        foreach var entity in xmember:File:EntityList
            if entity:Kind:IsPPSymbol()
                var ppentity := (XSourceMemberSymbol) entity
                sb:AppendLine(ppentity:SourceCode)
                ++nLines
            endif
            if entity:Range:StartLine >= xmember:Range:StartLine
                exit
            endif
        next

        IF owner IS XSourceTypeSymbol VAR td .AND. td:Name != XLiterals.GlobalName .AND. td:Kind == Kind.Class
            sb:AppendLine(td:SourceCode)
            nLines += 1
            startIndex  -= sb:Length
            startLine   -= nLines
            sb:AppendLine(source)
            sb:AppendLine()
            IF td:ClassType == XSharpDialect.XPP
                sb:AppendLine("ENDCLASS")
            ELSEIF td:ClassType == XSharpDialect.FoxPro
                sb:AppendLine("ENDDEFINE")
            ELSE
                sb:AppendLine("END CLASS")
            ENDIF
        ELSE
            startIndex  -= sb:Length
            startLine   -= nLines
            sb:Append(source)
        ENDIF

        SELF:Parse(sb:ToString(), TRUE)
        VAR result := List<XSourceVariableSymbol>{}
        FOREACH VAR xVar IN SELF:_locals
            xVar:Range     := xVar:Range:AddLine(startLine)
            xVar:Interval  := xVar:Interval:AddPos(startIndex)
            result:Add(xVar)
        NEXT
        RETURN result

    METHOD ParseBlocks(tokens AS IList<IToken>) AS VOID
        SELF:ParseTokens(tokens, TRUE, FALSE)

    INTERNAL METHOD ParseTokens(tokens AS IList<IToken> , lIncludeRegions AS LOGIC, lIncludeLocals AS LOGIC) AS VOID
        IF SELF:ParseOptions == NULL
            RETURN
        ENDIF
        WriteOutputMessage("-->> ParseTokens() "+SELF:SourcePath+" locals "+lIncludeLocals:ToString()+" )")
        TRY
            var tree := SELF:AntlrParse(_source, out var stream)
            VAR parser := XsParser{_file, SELF:ParseOptions:Dialect}
            parser:SaveToDisk := SELF:SaveToDisk
            WriteOutputMessage("-->> ParseTokens() "+SELF:SourcePath+" ManualParse Start")
            parser:Parse(tokens , lIncludeRegions, lIncludeLocals)
            WriteOutputMessage("-->> ParseTokens() "+SELF:SourcePath+" ManualParse End")
            SELF:_entities := parser:EntityList
            SELF:_blocks   := parser:BlockList
            SELF:_locals   := parser:Locals

        CATCH e AS Exception
            WriteOutputMessage(SELF:SourcePath)
            XSettings.Exception(e, __FUNCTION__)
        END TRY
        WriteOutputMessage("<<-- ParseTokens() "+SELF:SourcePath)

    METHOD Parse(cSource AS STRING) AS VOID
        SELF:Parse(cSource, FALSE)
        RETURN

    METHOD Parse(lIncludeLocals AS LOGIC) AS VOID
        IF System.IO.File.Exists(SELF:SourcePath)
            VAR cSource      := System.IO.File.ReadAllText(SELF:SourcePath)
            SELF:Parse(cSource, lIncludeLocals)
        ENDIF

    METHOD Parse(cSource AS STRING, lIncludeLocals AS LOGIC) AS VOID
        IF SELF:_file == NULL
            RETURN
        ENDIF
        WriteOutputMessage("-->> Parse() "+SELF:SourcePath+" locals "+lIncludeLocals:ToString()+" )")
        TRY
            VAR tree := SELF:AntlrParse(cSource, out var oStream)

            VAR stream   := SELF:Lex(cSource)
            if stream != null
                SELF:ParseTokens(stream:GetTokens(), FALSE, lIncludeLocals)
            endif
        CATCH e AS Exception
            WriteOutputMessage("Parse() Failed:")
            WriteOutputMessage(SELF:SourcePath)
            XSettings.Exception(e, __FUNCTION__)

        END TRY
        WriteOutputMessage("<<-- Parse() "+SELF:SourcePath)

#region Errors
    VIRTUAL METHOD ReportError(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
        SELF:_errors:Add(XError{fileName, span, errorCode, message, args})

    VIRTUAL METHOD ReportWarning(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
        SELF:_errors:Add(XWarning{fileName, span, errorCode, message, args})


        /*
        PRIVATE METHOD ShowErrorsAsync(syntaxRoot AS SyntaxNode) AS VOID
        LOCAL list AS List<XError>
        LOCAL obj2 AS OBJECT
        LOCAL sourcePath AS STRING
        LOCAL span AS LinePositionSpan
        LOCAL start AS LinePosition
        LOCAL length AS LONG
        //
        IF (SELF:_prjNode != NULL)
        //
        WriteOutputMessage("-->> ShowErrorsAsync() "+_file:FullPath)
        list := List<XError>(SELF:_errors)
        obj2 := SELF:_gate
        BEGIN LOCK obj2
        //
        sourcePath := SELF:_file:SourcePath
        SELF:_prjNode:ClearIntellisenseErrors(sourcePath)
        IF ((list != NULL) .AND. SELF:_prjNode:IsDocumentOpen(sourcePath))
        //
        FOREACH error AS XError IN list
        //
        span := error:Span
        start := span:Start
        length := ((span:@@End:Character - span:Start:Character) + 1)
        SELF:_prjNode:AddIntellisenseError(sourcePath, (start:Line + 1), (start:Character + 1), length, error:ErrCode, error:ToString(), error:Severity)
        NEXT
        ENDIF
        SELF:_prjNode:ShowIntellisenseErrors()
        END LOCK
        WriteOutputMessage("<<-- ShowErrorsAsync() "+_file:FullPath)
        ENDIF
        */
    STATIC METHOD WriteOutputMessage(message AS STRING) AS VOID
        IF XSettings.EnableParseLog .AND. XSettings.EnableLogging
            XSettings.Information("XModel.SourceWalker "+message)
        ENDIF

#endregion

END CLASS

END NAMESPACE

