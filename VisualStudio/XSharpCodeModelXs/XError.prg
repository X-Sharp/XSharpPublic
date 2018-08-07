//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING LanguageService.CodeAnalysis
USING LanguageService.CodeAnalysis.Text

BEGIN NAMESPACE XSharpModel
	CLASS XError
		
		// Methods
		CONSTRUCTOR(path AS STRING, span AS LinePositionSpan, errCode AS STRING, message AS STRING, PARAMS AS OBJECT[]);SUPER()
			//
			SELF:Path := path
			SELF:Span := span
			SELF:ErrCode := errCode
			SELF:Message := message
			SELF:Params := PARAMS
			SELF:Severity := DiagnosticSeverity.Error
		
		VIRTUAL METHOD ToString() AS STRING
			RETURN String.Format(SELF:Message, SELF:Params)
		
		
		// Properties
		PROPERTY ErrCode AS STRING AUTO 
		PROPERTY Message AS STRING AUTO 
		PROPERTY PARAMS AS OBJECT[] AUTO 
		PROPERTY Path AS STRING AUTO 
		PROPERTY Severity AS DiagnosticSeverity AUTO 
		PROPERTY Span AS LinePositionSpan AUTO 
		
	END CLASS
	
	CLASS XWarning INHERIT XError
		CONSTRUCTOR(path AS STRING, span AS LinePositionSpan, errCode AS STRING, message AS STRING, args AS OBJECT[])
			SUPER(path, span, errCode, message, args)
			SUPER:Severity := DiagnosticSeverity.Warning
		
		
	END CLASS
END NAMESPACE 

