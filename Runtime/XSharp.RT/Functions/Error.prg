//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/break/*" />
FUNCTION _Break(uValue AS USUAL) AS USUAL
	BREAK uValue



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/errorblock/*" />
FUNCTION ErrorBlock() AS USUAL STRICT
	LOCAL cbOld AS CODEBLOCK
	cbOld := XSharp.RuntimeState.GetValue<CODEBLOCK>(Set.ErrorBlock)
	IF cbOld == NULL_CODEBLOCK
		cbOld := {|e| DefError(  e ) }
		XSharp.RuntimeState.SetValue<CODEBLOCK>(Set.ErrorBlock, cbOld)
	ENDIF
	RETURN cbOld


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/errorblock/*" />
FUNCTION ErrorBlock(cbNewSetting AS CODEBLOCK) AS USUAL
	LOCAL cbOld AS CODEBLOCK
	cbOld := XSharp.RuntimeState.GetValue<CODEBLOCK>(Set.ErrorBlock)
	XSharp.RuntimeState.SetValue<CODEBLOCK>(Set.ErrorBlock, cbNewSetting)
	RETURN cbOld



 
 INTERNAL FUNCTION DefError(oError AS Error) AS USUAL
	// Add check from VO Default Error handler to avoid throwing errors for
	// some RDD related operations
	IF oError:CanDefault
		// network open error?
		IF (oError:GenCode = EG_OPEN)
			IF (oError:OsCode  = 32 ) .OR.; // ERROR_SHARING_VIOLATION
				(oError:OsCode  = 33 )    .OR.; // ERROR_LOCK_VIOLATION
				(oError:GenCode = EG_APPENDLOCK)
				NetErr(.T.)
				RETURN FALSE                // continue with default behavior
			ENDIF
		ENDIF
	ENDIF
	THROW oError

/// <exclude />
FUNCTION VO_Sprintf( format AS USUAL,  args PARAMS OBJECT[] ) AS STRING
    IF format:IsString
	    RETURN _VO_Sprintf( (STRING) format, args)
    ENDIF
    RETURN _VO_Sprintf( (DWORD) format, args)
