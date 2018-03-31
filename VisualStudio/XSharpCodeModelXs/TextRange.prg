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
    /// <summary>
    /// 1 based TextRange
    /// </summary>
	[DebuggerDisplay("{StartLine}.{StartColumn}-{EndLine}.{EndColumn}")];
	structure TextRange
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
		
		static property Empty as TextRange get TextRange{1, 1, 1, 1}
		
        /// <summary>
        /// 1 based Start Line
        /// </summary>
		property StartLine as long get self:_StartLine
        /// <summary>
        /// 1 based End Line
        /// </summary>
		property EndLine as long get self:_EndLine
        /// <summary>
        /// 1 based Start Column
        /// </summary>
		property StartColumn as long get self:_StartColumn
        /// <summary>
        /// 1 based End Column
        /// </summary>
		property EndColumn as long get self:_EndColumn
		
		
		method ContainsExclusive(line as long, col as long) as logic
			if ((line > self:_StartLine) .AND. (line < self:_EndLine))
				return true
			endif
			if (line == self:_StartLine)
				if (col > self:_StartColumn)
					if (line < self:_EndLine)
						return true
					endif
					if (line == self:_EndLine)
						return (col < self:_EndColumn)
					endif
				endif
				return false
			endif
			return ((line == self:_EndLine) .AND. (col < self:_EndColumn))
		
		method ContainsInclusive(line as long, col as long) as logic
			if ((line > self:_StartLine) .AND. (line < self:_EndLine))
				return true
			endif
			if (line == self:_StartLine)
				if (col >= self:_StartColumn)
					//
					if (line < self:_EndLine)
						return true
					endif
					if (line == self:_EndLine)
						return (col <= self:_EndColumn)
					endif
				endif
				return false
			endif
			return ((line == self:_EndLine) .AND. (col <= self:_EndColumn))
		
		
		
		
		
	end structure
	
end namespace 

