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
begin namespace XSharpModel
	[DebuggerDisplay("{Start}-{Stop}")];
	structure TextInterval
		// Fields
		private initonly _StartIndex as long
		private initonly _StopIndex as long
		
		// Constructors
		
		constructor(start as long, stop as long)
			//
			self:_StartIndex := start
			self:_StopIndex := stop
		
		
		static property Empty as TextInterval get TextInterval{}


		method IsEmpty() as logic
			return ((self:_StartIndex == 0) .AND. (self:_StopIndex == 0))
		
        /// <summary>
        /// 0 based StartIndex
        /// </summary>
		property Start as long get self:_StartIndex

        /// <summary>
        /// 0 based StopIndex
        /// </summary>
		property Stop as long get self:_StopIndex
		
		property Width as long get self:_StopIndex - self:_StartIndex + 1

		method ContainsInclusive(position as long) as logic
			if position >= self:_StartIndex  .AND. position <= self:_StopIndex
				return true
			endif
			return false
		
		method ContainsExclusive(position as long) as logic
			if position > self:_StartIndex .AND. position < self:_StopIndex
				return true
			endif
			return  false
		
	end structure
	
end namespace 

