//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.SyntaxTree
using System.Collections.Generic
using System.Linq
using System.Text
using System.Threading.Tasks
using System.Diagnostics
using System
using System.Runtime.InteropServices
using System
begin namespace XSharpModel
	[StructLayout(LayoutKind.Sequential), DebuggerDisplay("{StartLine}.{StartColumn}-{EndLine}.{EndColumn}")];
		structure TextRange
		// Fields
		initonly private _EndColumn as long
		initonly private _EndLine as long
		initonly private _StartColumn as long
		initonly private _StartLine as long
		
		// Methods
		//CONSTRUCTOR(context AS ParserRuleContext)
		//SELF(context:Start:Line, context:Start:Column, context:Stop:Line, context:Stop:Column)
		//
		
		constructor(sl as long, sc as long, el as long, ec as long)
			//
			self:_StartLine := sl
			self:_StartColumn := sc
			self:_EndLine := el
			self:_EndColumn := ec
		
		method ContainsExclusive(line as long, col as long) as logic
			//
			if ((line > self:_StartLine) .AND. (line < self:_EndLine))
				//
				return true
			endif
			if (line == self:_StartLine)
				//
				if (col > self:_StartColumn)
					//
					if (line < self:_EndLine)
						//
						return true
					endif
					if (line == self:_EndLine)
						//
						return (col < self:_EndColumn)
					endif
				endif
				return false
			endif
			return ((line == self:_EndLine) .AND. (col < self:_EndColumn))
		
		method ContainsInclusive(line as long, col as long) as logic
			//
			if ((line > self:_StartLine) .AND. (line < self:_EndLine))
				//
				return true
			endif
			if (line == self:_StartLine)
				//
				if (col >= self:_StartColumn)
					//
					if (line < self:_EndLine)
						//
						return true
					endif
					if (line == self:_EndLine)
						//
						return (col <= self:_EndColumn)
					endif
				endif
				return false
			endif
			return ((line == self:_EndLine) .AND. (col <= self:_EndColumn))
		
		
		// Properties
		static property Empty as TextRange
			get
				//
				return TextRange{1, 1, 1, 1}
			end get
		end property
		
		property EndColumn as long
			get
				//
				return self:_EndColumn
			end get
		end property
		
		property EndLine as long
			get
				//
				return self:_EndLine
			end get
		end property
		
		property StartColumn as long
			get
				//
				return self:_StartColumn
			end get
		end property
		
		property StartLine as long
			get
				//
				return self:_StartLine
			end get
		end property
		
		
	end structure
	
end namespace 

