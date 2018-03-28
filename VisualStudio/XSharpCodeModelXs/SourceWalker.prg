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

begin namespace XSharpModel
	class SourceWalker implements IDisposable //, VsParser.IErrorListener
		// Fields
		private _errors as IList<XError>
		private _file as XFile
		private _gate as object
		private _prjNode as IXSharpProject
		private _snapshot as ITextSnapshot
		private _source as string
		private _tokenStream as ITokenStream
		private _tree as XSharpParser.SourceContext
		
		// Methods
		constructor(file as XFile);super()
			local sourcePath as string
			//
			self:_gate := object{}
			self:_file := file
			self:_prjNode := self:_file?:Project?:ProjectNode
			sourcePath := self:_file:SourcePath
			if (System.IO.File.Exists(sourcePath))
				//
				self:_source := System.IO.File.ReadAllText(sourcePath)
			endif
		
		constructor(file as XFile, snapshot as ITextSnapshot);super()
			//
			self:_gate := object{}
			self:_file := file
			self:_prjNode := self:_file?:Project?:ProjectNode
			self:Snapshot := snapshot
		
		constructor(file as XFile, source as string);super()
			//
			self:_gate := object{}
			self:_file := file
			self:_prjNode := self:_file?:Project?:ProjectNode
			self:_snapshot := null
			self:_source := source
		
		method BuildModel(xTree as XSharpParser.SourceContext, buildLocals as logic) as void
			//LOCAL discover AS XSharpModelDiscover
			//
			if (((self:_prjNode != null) .AND. self:_file:Project:Loaded) .AND. (xTree != null))
				//
				try
					//
					//IF (buildLocals)
					////
					//discover := XSharpModelDiscoverWithLocals{SELF:_file, xTree, SELF:_errors}
					//ELSE
					////
					//discover := XSharpModelDiscover{SELF:_file, xTree, SELF:_errors}
					//ENDIF
					////ParseTreeWalker{}:Walk(discover, xTree)
				catch exception as System.Exception
					//
					Support.Debug("SourceWalker.BuildModel failed: "+  exception:Message)
				end try
			endif
		
		virtual method Dispose() as void
			//
			self:Dispose(true)
		
		protected virtual method Dispose(disposing as logic) as void
			//
			if (disposing)
				//
				self:_errors := null
				self:_file := null
				self:_prjNode := null
				self:_tree := null
				self:_tokenStream := null
				self:_snapshot := null
			endif
		
		method Lex() as ITokenStream
			//LOCAL stream := NULL AS ITokenStream
			//LOCAL flag := FALSEAS Logic
			//LOCAL walker := NULL AS SourceWalker
			////
			//System.Diagnostics.Trace.WriteLine("-->> SourceWalker.Lex()")
			//SELF:_errors := List<XError>{}
			////flag := VsParser.Lex(SELF:_source, SELF:_file:SourcePath, SELF:_file:Project:ProjectNode:ParseOptions, SELF, out stream)
			//walker := SELF
			//BEGIN LOCK walker
			////
			//SELF:_hasParseErrors := ! flag
			//IF (flag)
			////
			//SELF:_tree := null
			//SELF:_tokenStream := stream
			//ELSE
			////
			//SELF:_tree := null
			//SELF:_tokenStream := null
			//SELF:_hasParseErrors := FALSE
			//ENDIF
			//SELF:_file:HasParseErrors := SELF:_hasParseErrors
			//END LOCK
			//System.Diagnostics.Trace.WriteLine("<<-- SourceWalker.Lex()")
			return self:_tokenStream
		
		method Parse() as XSharpParser.SourceContext
			//LOCAL stream := NULL AS ITokenStream
			//LOCAL context := NULL AS XSharpParser.SourceContext
			//LOCAL options AS XSharpParseOptions
			//LOCAL flag AS Logic
			//LOCAL walker AS SourceWalker
			////
			//System.Diagnostics.Trace.WriteLine("-->> SourceWalker.Parse()")
			//IF (SELF:_file:Project:ProjectNode:ParseOptions == null)
			////
			//options := XSharpParseOptions.Default
			//ENDIF
			//SELF:_errors := List<XError>{}
			////flag := VsParser.Parse(SELF:_source, SELF:_file:SourcePath, SELF:_file:Project:ProjectNode:ParseOptions, SELF, out stream, out context)
			//walker := SELF
			//BEGIN LOCK walker
			////
			//SELF:_hasParseErrors := ! flag
			//IF (flag)
			////
			//SELF:_tokenStream := stream
			//SELF:_tree := context
			//ELSE
			////
			//SELF:_tokenStream := null
			//SELF:_tree := null
			//ENDIF
			//SELF:_file:HasParseErrors := SELF:_hasParseErrors
			//END LOCK
			//System.Diagnostics.Trace.WriteLine("<<-- SourceWalker.Parse()")
			return self:_tree
		
		virtual method ReportError(fileName as string, span as LinePositionSpan, errorCode as string, message as string, args as object[]) as void
			//
			self:_errors:Add(XError{fileName, span, errorCode, message, args})
		
		virtual method ReportWarning(fileName as string, span as LinePositionSpan, errorCode as string, message as string, args as object[]) as void
			//
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
		
		
		// Properties
		property HasParseErrors as logic auto
		
		private property parseOptions as XSharpParseOptions
			get
				//
				return self:_prjNode:ParseOptions
			end get
		end property
		
		property Snapshot as ITextSnapshot
			get
				//
				return self:_snapshot
			end get
			set
				//
				self:_snapshot := value
				self:_source := self:_snapshot:GetText()
				self:_tree := null
				self:_tokenStream := null
			end set
		end property
		
		property TokenStream as ITokenStream
			get
				//
				return self:_tokenStream
			end get
		end property
		
		property Tree as XSharpParser.SourceContext
			get
				//
				return self:_tree
			end get
		end property
		
		
	end class
	
end namespace 

