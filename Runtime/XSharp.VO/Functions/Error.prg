//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//


/// <summary>Break out of the current BEGIN SEQUENCE ... END</summary>
/// <param name="uError"></param>
/// <returns>
/// </returns>
FUNCTION _Break(uError AS USUAL) AS USUAL
	BREAK uError
RETURN 	NIL   



/// <summary>
/// Return and optionally change the code block that is executed when a runtime error occurs.
/// </summary>
/// <param name="cobError"></param>
/// <returns>
/// </returns>
FUNCTION ErrorBlock(cobError AS CODEBLOCK) AS USUAL
	LOCAL cbOld AS CODEBLOCK
	cbOld := XSharp.RuntimeState.GetValue<CODEBLOCK>(Set.ErrorBlock)
	XSharp.RuntimeState.SetValue<CODEBLOCK>(Set.ErrorBlock, cobError)
	RETURN cbOld

FUNCTION ErrorBlock() AS USUAL STRICT
	LOCAL cbOld AS CODEBLOCK
	cbOld := XSharp.RuntimeState.GetValue<CODEBLOCK>(Set.ErrorBlock)
	IF cbOld == NULL_CODEBLOCK
		cbOld := {|e| DefError(  e ) }
		XSharp.RuntimeState.SetValue<CODEBLOCK>(Set.ErrorBlock, cbOld)
	ENDIF
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
	RETURN NIL
