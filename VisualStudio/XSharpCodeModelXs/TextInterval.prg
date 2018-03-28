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
	[StructLayout(LayoutKind.Sequential), DebuggerDisplay("{Start}-{Stop}")];
		structure TextInterval
		// Fields
		initonly private _StartIndex as long
		initonly private _StopIndex as long
		
		// Methods
		//CONSTRUCTOR(context AS ParserRuleContext)
		//SELF(context:Start:StartIndex, context:Stop:StopIndex)
		
		
		constructor(start as long, stop as long)
			//
			self:_StartIndex := start
			self:_StopIndex := stop
		
		method ContainsExclusive(position as long) as logic
			//
			return ((position > self:_StartIndex) .AND. (position < self:_StopIndex))
		
		method ContainsInclusive(position as long) as logic
			//
			return ((position >= self:_StartIndex) .AND. (position <= self:_StopIndex))
		
		method IsEmpty() as logic
			//
			return ((self:_StartIndex == 0) .AND. (self:_StopIndex == 0))
		
		
		// Properties
		static property Empty as TextInterval
			get
				//
				return TextInterval{}
			end get
		end property
		
		property Start as long
			get
				//
				return self:_StartIndex
			end get
		end property
		
		property Stop as long
			get
				//
				return self:_StopIndex
			end get
		end property
		
		property Width as long
			get
				//
				return ((self:_StopIndex - self:_StartIndex) + 1)
			end get
		end property
		
		
	end structure
	
end namespace 

