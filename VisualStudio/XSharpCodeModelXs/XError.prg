//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
using LanguageService.CodeAnalysis
using LanguageService.CodeAnalysis.Text

begin namespace XSharpModel
	class XError
		
		// Methods
		constructor(path as string, span as LinePositionSpan, errCode as string, message as string, params as object[]);super()
			//
			self:Path := path
			self:Span := span
			self:ErrCode := errCode
			self:Message := message
			self:Params := params
			self:Severity := DiagnosticSeverity.Error
		
		virtual method ToString() as string
			return String.Format(self:Message, self:Params)
		
		
		// Properties
		property ErrCode as string auto 
		property Message as string auto 
		property Params as object[] auto 
		property Path as string auto 
		property Severity as DiagnosticSeverity auto 
		property Span as LinePositionSpan auto 
		
	end class
	
	class XWarning inherit XError
		constructor(path as string, span as LinePositionSpan, errCode as string, message as string, args as object[])
			super(path, span, errCode, message, args)
			super:Severity := DiagnosticSeverity.Warning
		
		
	end class
end namespace 

