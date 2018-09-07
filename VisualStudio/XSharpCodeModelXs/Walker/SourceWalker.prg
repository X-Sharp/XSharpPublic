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
USING System.Collections.Immutable
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
			PRIVATE _gate AS OBJECT
			PRIVATE _prjNode AS IXSharpProject
			PRIVATE _tokenStream AS ITokenStream
			PRIVATE _info AS ParseResult

		#endregion
		#region Properties
			PROPERTY HasParseErrors AS LOGIC AUTO

			PRIVATE PROPERTY parseOptions AS XSharpParseOptions GET SELF:_prjNode:ParseOptions

			PROPERTY TokenStream AS ITokenStream GET SELF:_tokenStream


		#endregion

		CONSTRUCTOR(file AS XFile)
			SUPER()
			LOCAL sourcePath AS STRING
			SELF:_gate := OBJECT{}
			SELF:_file := file
			SELF:_prjNode := SELF:_file?:Project?:ProjectNode
			//
			sourcePath := SELF:_file:SourcePath



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
			WriteOutputMessage("-->> Lex() "+_file:FullPath)
			SELF:_errors := List<XError>{}
			LOCAL stream := NULL AS ITokenStream
			XSharp.Parser.VsParser.Lex(cSource, SELF:_file:SourcePath, SELF:_file:Project:ProjectNode:ParseOptions, SELF, OUT stream)
			BEGIN LOCK SELF
				SELF:_tokenStream := stream
			END LOCK
			WriteOutputMessage("<<-- Lex() "+_file:FullPath)
			RETURN stream

		PRIVATE METHOD createLines(cSource AS STRING) AS List<STRING>
			VAR lines := List<STRING>{}
			BEGIN USING VAR reader := StringReader{cSource}
				LOCAL cLine AS STRING
				DO WHILE (cLine := reader:ReadLine()) != NULL
					lines.Add(cLine)
				ENDDO
			END USING
			RETURN lines

			//method Parse(cSource as string, lIncludeLocals as LOGIC) as ParseResult
			//var lines := createLines(cSource)
			//return self:Parse(lines, lIncludeLocals)

		METHOD Parse(lines AS IList<STRING> , lIncludeLocals AS LOGIC) AS ParseResult
			WriteOutputMessage("-->> Parse() "+_file:FullPath+"(# lines " +lines:Count+" locals "+lIncludeLocals+" )")

			VAR oParser := XSharpModel.Parser{}
			VAR info := oParser:Parse(lines, lIncludeLocals)
			BEGIN LOCK SELF
				SELF:_info := info
			END LOCK
			WriteOutputMessage("<<-- Parse() "+_file:FullPath)
			RETURN SELF:_info


		#region Errors
			VIRTUAL METHOD ReportError(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
				SELF:_errors:Add(XError{fileName, span, errorCode, message, args})

			VIRTUAL METHOD ReportWarning(fileName AS STRING, span AS LinePositionSpan, errorCode AS STRING, message AS STRING, args AS OBJECT[]) AS VOID
				SELF:_errors:Add(XWarning{fileName, span, errorCode, message, args})

			PRIVATE METHOD ShowErrorsAsync(syntaxRoot AS SyntaxNode) AS VOID
				LOCAL list AS System.Collections.Immutable.ImmutableList<XError>
				LOCAL obj2 AS OBJECT
				LOCAL sourcePath AS STRING
				LOCAL span AS LinePositionSpan
				LOCAL start AS LinePosition
				LOCAL length AS LONG
				//
				IF (SELF:_prjNode != NULL)
					//
					WriteOutputMessage("-->> ShowErrorsAsync() "+_file:FullPath)
					list := System.Collections.Immutable.ImmutableList.ToImmutableList<XError>(SELF:_errors)
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
		STATIC METHOD WriteOutputMessage(message AS STRING) AS VOID
			XSolution.WriteOutputMessage("XModel.SourceWalker "+message)

		#endregion

	END CLASS

END NAMESPACE

