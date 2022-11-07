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
	[DebuggerDisplay("{DebuggerDisplay(),nq}")];
	STRUCTURE TextInterval
		// Fields
		PRIVATE INITONLY _StartIndex AS LONG
		PRIVATE INITONLY _StopIndex AS LONG

		// Constructors

		CONSTRUCTOR(start AS LONG, stop AS LONG)
			//
			SELF:_StartIndex := Math.Max(start,0)
			SELF:_StopIndex := Math.Max(stop,0)

		CONSTRUCTOR(startToken AS IToken, endToken AS IToken)
			//
			SELF:_StartIndex := Math.Max(startToken:StartIndex,0)
			SELF:_StopIndex  := Math.Max(endToken:StopIndex,0)

		STATIC PROPERTY Empty AS TextInterval GET TextInterval{}

      METHOD WithEnd (endToken AS IToken) AS TextInterval
          RETURN TextInterval{SELF:_StartIndex, endToken:StopIndex}

      METHOD AddPos(pos AS INT) AS TextInterval
         RETURN TextInterval{SELF:_StartIndex+pos, SELF:_StopIndex+pos}

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

      METHOD DebuggerDisplay() AS STRING
         RETURN SELF:ToString()

      OVERRIDE METHOD ToString() AS STRING
         RETURN i"{Start}-{Stop}"

	END STRUCTURE

END NAMESPACE

