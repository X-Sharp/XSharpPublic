//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//
USING LanguageService.SyntaxTree
USING System.Collections.Generic
USING System.Linq
USING System.Text
USING System.Threading.Tasks
USING System.Diagnostics
USING System
USING System.Runtime.InteropServices
USING System
BEGIN NAMESPACE XSharpModel
    /// <summary>
    /// 1 based TextRange
    /// </summary>
	[DebuggerDisplay("{StartLine}.{StartColumn}-{EndLine}.{EndColumn}")];
	STRUCTURE TextRange
		INITONLY PRIVATE _EndColumn AS LONG
		INITONLY PRIVATE _EndLine AS LONG
		INITONLY PRIVATE _StartColumn AS LONG
		INITONLY PRIVATE _StartLine AS LONG
		
		// Methods
		//CONSTRUCTOR(context AS ParserRuleContext)
		//SELF(context:Start:Line, context:Start:Column, context:Stop:Line, context:Stop:Column)
		//
		
		CONSTRUCTOR(sl AS LONG, sc AS LONG, el AS LONG, ec AS LONG)
			//
			SELF:_StartLine := sl
			SELF:_StartColumn := sc
			SELF:_EndLine := el
			SELF:_EndColumn := ec
		
		STATIC PROPERTY Empty AS TextRange GET TextRange{1, 1, 1, 1}
		
        /// <summary>
        /// 1 based Start Line
        /// </summary>
		PROPERTY StartLine AS LONG GET SELF:_StartLine
        /// <summary>
        /// 1 based End Line
        /// </summary>
		PROPERTY EndLine AS LONG GET SELF:_EndLine
        /// <summary>
        /// 1 based Start Column
        /// </summary>
		PROPERTY StartColumn AS LONG GET SELF:_StartColumn
        /// <summary>
        /// 1 based End Column
        /// </summary>
		PROPERTY EndColumn AS LONG GET SELF:_EndColumn
		
		
		METHOD ContainsExclusive(line AS LONG, col AS LONG) AS LOGIC
			IF ((line > SELF:_StartLine) .AND. (line < SELF:_EndLine))
				RETURN true
			ENDIF
			IF (line == SELF:_StartLine)
				IF (col > SELF:_StartColumn)
					IF (line < SELF:_EndLine)
						RETURN true
					ENDIF
					IF (line == SELF:_EndLine)
						RETURN (col < SELF:_EndColumn)
					ENDIF
				ENDIF
				RETURN false
			ENDIF
			RETURN ((line == SELF:_EndLine) .AND. (col < SELF:_EndColumn))
		
		METHOD ContainsInclusive(line AS LONG, col AS LONG) AS LOGIC
			IF ((line > SELF:_StartLine) .AND. (line < SELF:_EndLine))
				RETURN true
			ENDIF
			IF (line == SELF:_StartLine)
				IF (col >= SELF:_StartColumn)
					//
					IF (line < SELF:_EndLine)
						RETURN true
					ENDIF
					IF (line == SELF:_EndLine)
						RETURN (col <= SELF:_EndColumn)
					ENDIF
				ENDIF
				RETURN false
			ENDIF
			RETURN ((line == SELF:_EndLine) .AND. (col <= SELF:_EndColumn))
		
		
		
		
		
	END STRUCTURE
	
END NAMESPACE 

