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
			PRIVATE _errors AS IList<XError>
			PRIVATE _file AS XFile
			//PRIVATE _gate AS OBJECT
			PRIVATE _prjNode AS IXSharpProject
			PRIVATE _tokenStream AS ITokenStream
			//PRIVATE _info AS ParseResult
         PRIVATE _entities AS IList<XEntityDefinition>
         PRIVATE _blocks   AS IList<XBlock>
         PRIVATE _locals   AS IList<XVariable>

		#endregion
		#region Properties
			PROPERTY HasParseErrors AS LOGIC AUTO

			PRIVATE PROPERTY parseOptions AS XSharpParseOptions GET SELF:_prjNode:ParseOptions

			PROPERTY TokenStream AS ITokenStream GET SELF:_tokenStream

			PROPERTY StartPosition AS INT AUTO

         PROPERTY EntityList AS IList<XEntityDefinition> GET _entities
         PROPERTY BlockList  AS IList<XBlock>   GET _blocks
         PROPERTY File AS XFile GET _file

		#endregion

		CONSTRUCTOR(file AS XFile)
			SUPER()
            IF (file != NULL)
			    LOCAL sourcePath AS STRING
			    //SELF:_gate := OBJECT{}
			    SELF:_file := file
			    SELF:_prjNode := SELF:_file?:Project?:ProjectNode
			    SELF:StartPosition := 0
			    //
			    sourcePath := SELF:_file:SourcePath
            ENDIF

      /*
		METHOD BuildModel(oInfo AS ParseResult) AS VOID
			IF SELF:_prjNode != NULL .AND. SELF:_file:Project:Loaded
				//
				TRY
					SELF:_file:BuildTypes(oInfo)
				CATCH exception AS System.Exception
					WriteOutputMessage("BuildModel failed: ")
					XSolution.WriteException(exception)
				END TRY
			ENDIF
      */
      
		VIRTUAL METHOD Dispose() AS VOID 
			SELF:Dispose(TRUE)

		PROTECTED VIRTUAL METHOD Dispose(disposing AS LOGIC) AS VOID
			IF disposing
				//
				SELF:_errors := NULL
				SELF:_file := NULL
				SELF:_prjNode := NULL
				SELF:_tokenStream := NULL
			ENDIF


		METHOD Lex(cSource AS STRING) AS ITokenStream
			LOCAL lOk := FALSE AS LOGIC
			//WriteOutputMessage("-->> Lex() "+_file:FullPath)
			SELF:_errors := List<XError>{}
			LOCAL stream := NULL AS ITokenStream
            TRY
			XSharp.Parser.VsParser.Lex(cSource, SELF:_file:SourcePath, SELF:_file:Project:ParseOptions, SELF, OUT stream)
			BEGIN LOCK SELF
				SELF:_tokenStream := stream
			END LOCK
            CATCH e AS Exception
                WriteOutputMessage("Lex() Failed:")
                WriteOutputMessage(_file:FullPath)                  
                WriteOutputMessage(e:ToString())
            END TRY
			//WriteOutputMessage("<<-- Lex() "+_file:FullPath)
			RETURN stream

      METHOD ParseLocals(source as STRING, startLine AS INT, startIndex AS INT) AS List<IXVariable>
         // This is JUST the source of the method. The locations in the variables need to be adjusted
         SELF:ParseNew(source, TRUE)         
         VAR result := List<IXVariable>{}
         startLine += 1
         FOREACH VAR xVar IN SELF:_locals
            xVar:Range     := TextRange{startLine, xVar:Range:StartColumn, xVar:Range:EndLine+startLine, xVar:Range:EndColumn}
            xVar:Interval  := TextInterval{startIndex+xVar:Interval:Start, startIndex+xVar:Interval:Stop}
            result:Add(xVar)
         NEXT
         RETURN result
         


      METHOD ParseTokens(tokens AS ITokenStream , lIncludeRegions AS LOGIC, lIncludeLocals AS LOGIC) AS VOID

         //WriteOutputMessage("-->> ParseTokens() "+_file:FullPath+" locals "+lIncludeLocals:ToString()+" )")
         TRY
            VAR parser := XsParser{_file}
            parser:Parse(tokens , lIncludeRegions, lIncludeLocals)
            SELF:_entities := parser:EntityList
            SELF:_blocks   := parser:BlockList
            SELF:_locals   := parser:Locals
            
         CATCH e AS Exception
               WriteOutputMessage("ParseTokens() Failed:")
               WriteOutputMessage(_file:FullPath)
               WriteOutputMessage(e:ToString())
            
         END TRY
         //WriteOutputMessage("<<-- ParseTokens() "+_file:FullPath)


      METHOD ParseNew(lIncludeLocals AS LOGIC) AS VOID
         VAR cSource      := System.IO.File.ReadAllText(_file:FullPath)
         SELF:ParseNew(cSource, lIncludeLocals)

      METHOD ParseNew(cSource AS STRING, lIncludeLocals AS LOGIC) AS VOID
         //WriteOutputMessage("-->> ParseNew() "+_file:FullPath+" locals "+lIncludeLocals:ToString()+" )")
         TRY
            VAR tokens   := SELF:Lex(cSource)
            SELF:ParseTokens(tokens, FALSE, lIncludeLocals)
         CATCH e AS Exception
            WriteOutputMessage("ParseNew() Failed:")
            WriteOutputMessage(_file:FullPath)
            WriteOutputMessage(e:ToString())
            
         END TRY
         //WriteOutputMessage("<<-- ParseNew() "+_file:FullPath)

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
			XSolution.WriteOutputMessage("XModel.SourceWalker "+message)

		#endregion

	END CLASS

END NAMESPACE

