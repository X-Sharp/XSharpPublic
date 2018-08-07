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
BEGIN NAMESPACE XSharpModel
	[DebuggerDisplay("{Start}-{Stop}")];
	STRUCTURE TextInterval
		// Fields
		PRIVATE INITONLY _StartIndex AS LONG
		PRIVATE INITONLY _StopIndex AS LONG
		
		// Constructors
		
		CONSTRUCTOR(start AS LONG, stop AS LONG)
			//
			SELF:_StartIndex := start
			SELF:_StopIndex := stop
		
		
		STATIC PROPERTY Empty AS TextInterval GET TextInterval{}


		METHOD IsEmpty() AS LOGIC
			RETURN ((SELF:_StartIndex == 0) .AND. (SELF:_StopIndex == 0))
		
        /// <summary>
        /// 0 based StartIndex
        /// </summary>
		PROPERTY Start AS LONG GET SELF:_StartIndex

        /// <summary>
        /// 0 based StopIndex
        /// </summary>
		PROPERTY Stop AS LONG GET SELF:_StopIndex
		
		PROPERTY Width AS LONG GET SELF:_StopIndex - SELF:_StartIndex + 1

		METHOD ContainsInclusive(position AS LONG) AS LOGIC
			IF position >= SELF:_StartIndex  .AND. position <= SELF:_StopIndex
				RETURN true
			ENDIF
			RETURN false
		
		METHOD ContainsExclusive(position AS LONG) AS LOGIC
			IF position > SELF:_StartIndex .AND. position < SELF:_StopIndex
				RETURN true
			ENDIF
			RETURN  false
		
	END STRUCTURE
	
END NAMESPACE 

