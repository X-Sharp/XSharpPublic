//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis.XSharp
using System
using System.Collections.Generic
using System.Linq
using System.Text
using System.Threading.Tasks
using Microsoft.VisualStudio.Text
using Microsoft.VisualStudio.Text.Classification
using LanguageService.SyntaxTree
using LanguageService.CodeAnalysis.XSharp.SyntaxParser
using LanguageService.CodeAnalysis.XSharp.Syntax
using System.Collections.Immutable
using LanguageService.CodeAnalysis
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
			private _snapshot as ITextSnapshot
			private _lines  as IList<string>
			private _source as string
			private _tokenStream as ITokenStream
			private _info as ParseResult
			
		#endregion
		#region Properties
			property HasParseErrors as logic auto
			
			private property parseOptions as XSharpParseOptions get self:_prjNode:ParseOptions
			
			property Snapshot as ITextSnapshot
				get
					//
					return self:_snapshot
				end get
				set
					//
					self:_snapshot := value
					self:_source  := self:_snapshot:GetText()
					var aLines := List<string>{}
					foreach line as ITextSnapshotline in self:_snapshot:Lines
						aLines:Add(line:GetText())
					next
					self:_lines := aLines
					self:_tokenStream := null
				end set
			end property
			
			property TokenStream as ITokenStream get self:_tokenStream
			
			
		#endregion
		
		#region constructors
			constructor(file as XFile);super()
				local sourcePath as string
				//
				self:_gate := object{}
				self:_file := file
				self:_prjNode := self:_file?:Project?:ProjectNode
				sourcePath := self:_file:SourcePath
				if (System.IO.File.Exists(sourcePath))
					//
					self:_lines  := System.IO.File.ReadAllLines(sourcePath)
					self:_source := System.IO.File.ReadAllText(sourcePath)
				endif
			
			constructor(file as XFile, snapshot as ITextSnapshot)
				super()
				//
				self:_gate := object{}
				self:_file := file
				self:_prjNode := self:_file?:Project?:ProjectNode
				self:Snapshot := snapshot
			
			constructor(file as XFile, source as string)
				super()
				//
				self:_gate := object{}
				self:_file := file
				self:_prjNode := self:_file?:Project?:ProjectNode
				self:_snapshot := null
				self:_source := source
		#endregion
		
		method BuildModel(oInfo as ParseResult, buildLocals as logic) as void
			if self:_prjNode != null .AND. self:_file:Project:Loaded
				//
				try
					self:_file:BuildTypes(oInfo)
				catch exception as System.Exception
					Support.Debug("SourceWalker.BuildModel failed: "+  exception:Message)
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
				self:_snapshot := null
			endif
		
		method Lex() as ITokenStream
			local lOk := false as logic
			System.Diagnostics.Trace.WriteLine("-->> SourceWalker.Lex()")
			self:_errors := List<XError>{}
			begin lock self
				local stream := null as ITokenStream
				XSharp.Parser.VsParser.Lex(self:_source, self:_file:SourcePath, self:_file:Project:ProjectNode:ParseOptions, self, out stream)
				self:_tokenStream := stream
			end lock
			System.Diagnostics.Trace.WriteLine("<<-- SourceWalker.Lex()")
			return self:_tokenStream
		
		method Parse() as ParseResult
			System.Diagnostics.Trace.WriteLine("-->> SourceWalker.Parse()")
			
			var oParser := XSharpModel.Parser{}
			var info := oParser:Parse(self:_lines)
			begin lock self
				self:_info := info				
			end lock
			System.Diagnostics.Trace.WriteLine("<<-- SourceWalker.Parse()")
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
					System.Diagnostics.Trace.WriteLine("-->> SourceWalker.ShowErrorsAsync()")
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
					System.Diagnostics.Trace.WriteLine("<<-- SourceWalker.ShowErrorsAsync()")
				endif
			
		#endregion
		
	end class
	
end namespace 

