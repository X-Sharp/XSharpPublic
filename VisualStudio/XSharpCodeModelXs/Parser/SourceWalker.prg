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
USING STATIC XSharp.Parser.VsParser
USING LanguageService.CodeAnalysis.Text
USING XSharp.Parser

BEGIN NAMESPACE XSharpModel
    CLASS SourceWalker IMPLEMENTS IDisposable , VsParser.IErrorListener
        #region fields
        PRIVATE _errors     AS IList<XError>
        PRIVATE _file       AS XFile
        PRIVATE _prjNode    AS IXSharpProject
        PRIVATE _tokenStream AS ITokenStream
        PRIVATE _entities   AS IList<XSourceEntity>
        PRIVATE _blocks     AS IList<XSourceBlock>
        PRIVATE _locals     AS IList<XSourceVariableSymbol>
        PRIVATE _options    AS XSharpParseOptions

        #endregion
        #region Properties
        PROPERTY HasParseErrors AS LOGIC AUTO

        PRIVATE PROPERTY parseOptions AS XSharpParseOptions GET SELF:_prjNode:ParseOptions

        PROPERTY TokenStream AS ITokenStream GET SELF:_tokenStream

        PROPERTY StartPosition AS INT AUTO
        PROPERTY SourcePath AS STRING AUTO  // Save it because calculation the XAML source path is a bit expensive

        PROPERTY EntityList AS IList<XSourceEntity> GET _entities
        PROPERTY BlockList  AS IList<XSourceBlock>   GET _blocks
        PROPERTY File AS XFile GET _file
        PROPERTY SaveToDisk AS LOGIC AUTO
        #endregion

        CONSTRUCTOR(file AS XFile, lSaveResults := TRUE AS LOGIC )
            SUPER()
            SELF:SaveToDisk := lSaveResults
            IF (file != NULL)
                SELF:_file := file
                SELF:_prjNode := SELF:_file?:Project?:ProjectNode
                SELF:StartPosition := 0
                SELF:SourcePath := SELF:_file:SourcePath
                SELF:_options   := SELF:_file:Project:ParseOptions
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
                SELF:_prjNode := NULL
                SELF:_tokenStream := NULL
                SELF:_blocks   := NULL
                SELF:_locals   := NULL
            ENDIF
            IF SELF:SaveToDisk
               SELF:_entities := NULL
            ENDIF



        METHOD Lex(cSource AS STRING) AS ITokenStream
            LOCAL lOk := FALSE AS LOGIC
            WriteOutputMessage("-->> Lex() "+SourcePath)
            SELF:_errors := List<XError>{}
            LOCAL stream := NULL AS ITokenStream
            TRY
                XSharp.Parser.VsParser.Lex(cSource, SELF:SourcePath, _options, SELF, OUT stream)
                BEGIN LOCK SELF
                    SELF:_tokenStream := stream
                END LOCK
            CATCH e AS Exception
                WriteOutputMessage("Lex() Failed:")
                WriteOutputMessage(SELF:SourcePath)
                WriteOutputMessage(e:ToString())
            END TRY
            WriteOutputMessage("<<-- Lex() "+SELF:SourcePath)
            RETURN stream

        METHOD ParseLocals(source AS STRING, xmember AS XSourceMemberSymbol) AS List<XSourceVariableSymbol>
            // This is JUST the source of the method. The locations in the variables need to be adjusted

            VAR owner := xmember:Parent
            VAR startLine := xmember:Range:StartLine
            VAR startIndex := xmember:Interval:Start+1
            IF owner IS XSourceTypeSymbol VAR td .AND. td:Name != XLiterals.GlobalName .AND. td:Kind == Kind.Class
                source := td:SourceCode + e"\r\n" + source
                startLine   -= 1
                startIndex  -= (td:SourceCode:Length +2)
                IF td:ClassType == XSharpDialect.XPP
                    source += e"\r\nENDCLASS\r\n"
                ELSEIF td:ClassType == XSharpDialect.FoxPro
                    source += "\r\nENDDEFINE\r\n"
                ELSE
                    source += "\r\nEND CLASS\r\n"
                ENDIF
            ENDIF
            SELF:Parse(source, TRUE)
            VAR result := List<XSourceVariableSymbol>{}
            FOREACH VAR xVar IN SELF:_locals
                xVar:Range     := xVar:Range:AddLine(startLine)
                xVar:Interval  := xVar:Interval:AddPos(startIndex)
                result:Add(xVar)
            NEXT
            RETURN result



        METHOD ParseTokens(tokens AS ITokenStream , lIncludeRegions AS LOGIC, lIncludeLocals AS LOGIC) AS VOID
            WriteOutputMessage("-->> ParseTokens() "+SELF:SourcePath+" locals "+lIncludeLocals:ToString()+" )")
            TRY
                VAR parser := XsParser{_file, _options:Dialect}
                parser:SaveToDisk := SELF:SaveToDisk
                parser:Parse(tokens , lIncludeRegions, lIncludeLocals)
                SELF:_entities := parser:EntityList
                SELF:_blocks   := parser:BlockList
                SELF:_locals   := parser:Locals

            CATCH e AS Exception
                WriteOutputMessage("ParseTokens() Failed:")
                WriteOutputMessage(SELF:SourcePath)
                WriteOutputMessage(e:ToString())
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
                VAR tokens   := SELF:Lex(cSource)
                SELF:ParseTokens(tokens, FALSE, lIncludeLocals)
            CATCH e AS Exception
                WriteOutputMessage("Parse() Failed:")
                WriteOutputMessage(SELF:SourcePath)
                WriteOutputMessage(e:ToString())

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
                XSolution.WriteOutputMessage("XModel.SourceWalker "+message)
            ENDIF

        #endregion

        END CLASS

    END NAMESPACE

