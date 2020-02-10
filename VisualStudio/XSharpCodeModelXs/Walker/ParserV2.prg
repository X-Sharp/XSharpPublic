// ParserV2.prg
// Created by    : fabri
// Creation Date : 1/17/2020 5:53:39 PM
// Created for   : 
// WorkStation   : FABPORTABLE

//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Collections
USING System.Text
USING System.Text.RegularExpressions
USING System.IO
USING System.Diagnostics
USING System.Linq
USING LanguageService.CodeAnalysis.XSharp
USING LanguageService.SyntaxTree
USING LanguageService.CodeAnalysis.XSharp.SyntaxParser

BEGIN NAMESPACE XSharpModel


	PARTIAL CLASS ParserV2
		
		METHOD Parse( tokenStream AS ITokenStream, lIncludeLocals AS LOGIC ) AS VOID
			LOCAL lFirstKeyword AS LOGIC
			LOCAL currentLine AS INT
			LOCAL entity AS EntityObject
			//
			VAR stream := tokenStream ASTYPE BufferedTokenStream
			VAR allTokens := stream:GetTokens()
			currentLine := -1
			//
			FOREACH token AS IToken IN allTokens
				// Comments ?
				IF token:Channel == Lexer.Hidden
					LOOP
				ENDIF
				// New Line ?
				// -> Don't forget to check for ";" continuing line !!
				IF currentLine != token:Line
					currentLine := token:Line
					lFirstKeyword := TRUE
				ELSE
					lFirstKeyword := FALSE
				ENDIF
				// 
				IF lFirstKeyword
					//
					SWITCH token:Type
						CASE XSharpLexer.FUNCTION
							entity := EntityObject{ EntityType:_Function }
					END SWITCH
				ENDIF
				//


			NEXT
			
			
	END CLASS
	
END NAMESPACE