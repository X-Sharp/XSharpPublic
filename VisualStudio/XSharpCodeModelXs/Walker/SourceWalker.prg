//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using System
using System.IO
using System.Collections.Generic
using System.Linq
using System.Text
using System.Threading.Tasks
using LanguageService.SyntaxTree
using System.Collections.Immutable
using LanguageService.CodeAnalysis
using LanguageService.CodeAnalysis.XSharp
using static XSharp.Parser.VsParser
using LanguageService.CodeAnalysis.Text
using XSharp.Parser

begin namespace XSharpModel
	class SourceWalker implements IDisposable , VsParser.IErrorListener
		#region fields
			private _errors as IList<XError>
			private _file as XFile
			private _gate as object
			private _prjNode as IXSharpProject
			private _tokenStream as ITokenStream
			private _info as ParseResult
			
		#endregion
		#region Properties
			property HasParseErrors as logic auto
			
			private property parseOptions as XSharpParseOptions get self:_prjNode:ParseOptions
			
			property TokenStream as ITokenStream get self:_tokenStream
			
			
		#endregion
		
		constructor(file as XFile)
			super()
			local sourcePath as string
			self:_gate := object{}
			self:_file := file
			self:_prjNode := self:_file?:Project?:ProjectNode
			//
			sourcePath := self:_file:SourcePath
			
			
		
		method BuildModel(oInfo as ParseResult) as void
			if self:_prjNode != null .AND. self:_file:Project:Loaded
				//
				try
					self:_file:BuildTypes(oInfo)
				catch exception as System.Exception
					WriteOutputMessage("BuildModel failed: ")
					XSolution.WriteException(exception)
				end try
			endif
		
		virtual method Dispose() as void
			self:Dispose(true)
		
		protected virtual method Dispose(disposing as logic) as void
			if disposing
				//
				self:_errors := null
				self:_file := null
				self:_prjNode := null
				self:_tokenStream := null
			endif


		method Lex(cSource as string) as ITokenStream
			local lOk := false as logic
			WriteOutputMessage("-->> Lex() "+_file:FullPath)
			self:_errors := List<XError>{}
			local stream := null as ITokenStream
			XSharp.Parser.VsParser.Lex(cSource, self:_file:SourcePath, self:_file:Project:ProjectNode:ParseOptions, self, out stream)
			begin lock self
				self:_tokenStream := stream
			end lock
			WriteOutputMessage("<<-- Lex() "+_file:FullPath)
			return stream
		
		private method createLines(cSource as string) as List<String>
			var lines := List<String>{}
			begin using var reader := StringReader{cSource}
				local cLine as string
				do while (cLine := reader:ReadLine()) != null
					lines.Add(cLine)
				enddo
			end using
			return lines

			//method Parse(cSource as string, lIncludeLocals as LOGIC) as ParseResult
			//var lines := createLines(cSource)
			//return self:Parse(lines, lIncludeLocals)

		method Parse(lines as IList<String> , lIncludeLocals as LOGIC) as ParseResult
			WriteOutputMessage("-->> Parse() "+_file:FullPath+"(# lines " +lines:Count+" locals "+lIncludeLocals+" )")
			
			var oParser := XSharpModel.Parser{}
			var info := oParser:Parse(lines, lIncludeLocals)
			begin lock self
				self:_info := info				
			end lock
			WriteOutputMessage("<<-- Parse() "+_file:FullPath)
			return self:_info

		
		#region Errors		
			virtual method ReportError(fileName as string, span as LinePositionSpan, errorCode as string, message as string, args as object[]) as void
				self:_errors:Add(XError{fileName, span, errorCode, message, args})
			
			virtual method ReportWarning(fileName as string, span as LinePositionSpan, errorCode as string, message as string, args as object[]) as void
				self:_errors:Add(XWarning{fileName, span, errorCode, message, args})
			
			private method ShowErrorsAsync(syntaxRoot as SyntaxNode) as void
				local list as System.Collections.Immutable.ImmutableList<XError>
				local obj2 as object
				local sourcePath as string
				local span as LinePositionSpan
				local start as LinePosition
				local length as long
				//
				if (self:_prjNode != null)
					//
					WriteOutputMessage("-->> ShowErrorsAsync() "+_file:FullPath)
					list := System.Collections.Immutable.ImmutableList.ToImmutableList<XError>(self:_errors)
					obj2 := self:_gate
					begin lock obj2
						//
						sourcePath := self:_file:SourcePath
						self:_prjNode:ClearIntellisenseErrors(sourcePath)
						if ((list != null) .AND. self:_prjNode:IsDocumentOpen(sourcePath))
							//
							foreach error as XError in list
								//
								span := error:Span
								start := span:Start
								length := ((span:@@End:Character - span:Start:Character) + 1)
								self:_prjNode:AddIntellisenseError(sourcePath, (start:Line + 1), (start:Character + 1), length, error:ErrCode, error:ToString(), error:Severity)
							next
						endif
						self:_prjNode:ShowIntellisenseErrors()
					end lock
					WriteOutputMessage("<<-- ShowErrorsAsync() "+_file:FullPath)
				endif
		STATIC METHOD WriteOutputMessage(message AS STRING) AS void
			XSolution.WriteOutputMessage("XModel.SourceWalker "+message)
			
		#endregion
		
	end class
	
end namespace 

