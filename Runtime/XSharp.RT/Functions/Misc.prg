//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/between/*" />
FUNCTION Between(uValue AS USUAL,uMin AS USUAL,uMax AS USUAL) AS LOGIC
	RETURN uValue >=uMin .AND.  uValue<=uMax


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/inlist/*" />
FUNCTION InList(u AS USUAL, uValueList PARAMS USUAL[]) AS LOGIC
	RETURN _InListWorker(u, uValueList, FALSE)

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/inlistexact/*" />
FUNCTION InListExact(u AS USUAL, uValueList PARAMS USUAL[]) AS LOGIC
	RETURN _InListWorker(u, uValueList, TRUE)


INTERNAL FUNCTION _InListWorker( u AS USUAL, args AS CONST USUAL[], lExact AS LOGIC) AS LOGIC 
	LOCAL i, nLen AS INT
	nLen := args:Length
	IF lExact
		FOR i := 1 TO nLen
			IF args[i] == u
				RETURN TRUE
			ENDIF
		NEXT
	ELSE
		FOR i := 1 TO nLen
			IF u = args[i] 
				RETURN TRUE
			ENDIF
		NEXT
	ENDIF
	RETURN FALSE




/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/max/*" />
FUNCTION Max(uValue1 AS USUAL,uValue2 AS USUAL) AS USUAL

	IF uValue1:IsNumeric .AND. uValue2:IsNumeric

		IF uValue1:IsFloat .OR. uValue2:IsFloat
			RETURN (USUAL) Math.Max( (REAL8) uValue1, (REAL8) uValue2)

		ELSEIF uValue1:IsDecimal .OR. uValue2:IsDecimal
			RETURN (USUAL) Math.Max( (Decimal) uValue1, (Decimal) uValue2)

		ELSEIF uValue1:IsInt64 .OR. uValue2:IsInt64
			RETURN (USUAL) Math.Max( (INT64) uValue1, (INT64) uValue2)
		ENDIF
		RETURN (USUAL) Math.Max( (LONG) uValue1, (LONG) uValue2)

	ELSEIF uValue1:IsDate .AND. uValue2:IsDate
		RETURN IIF ((DATE) uValue1 > (DATE) uValue2, uValue1, uValue2)

	ELSEIF uValue1:IsDateTime .AND. uValue2:IsDateTime
		RETURN IIF ((DateTime) uValue1 > (DateTime) uValue2, uValue1, uValue2)

	ELSEIF (uValue1:IsDateTime .OR. uValue1:IsDate) .AND. (uValue2:IsDateTime .OR. uValue2:IsDate)
		RETURN IIF ((DateTime) uValue1 > (DateTime) uValue2, uValue1, uValue2)

	ELSEIF uValue1:IsString .AND. uValue2:IsString
		RETURN IIF ((STRING) uValue1 > (STRING) uValue2, uValue1, uValue2)

	ELSEIF uValue1:IsSymbol .AND. uValue2:IsSymbol
		RETURN IIF ((SYMBOL) uValue1 > (SYMBOL) uValue2, uValue1, uValue2)

	ELSE
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(uValue2) , "Incompatible types")
	ENDIF
	



/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/min/*" />
FUNCTION Min(uValue1 AS USUAL,uValue2 AS USUAL) AS USUAL
	IF uValue1:IsNumeric .AND. uValue2:IsNumeric

		IF uValue1:IsFloat .OR. uValue2:IsFloat
			
			RETURN (USUAL) Math.Min((REAL8) uValue1, (REAL8) uValue2)
		
		ELSEIF uValue1:IsDecimal .OR. uValue2:IsDecimal
			RETURN (USUAL) Math.Min( (Decimal) uValue1, (Decimal) uValue2)
		
		ELSEIF uValue1:IsInt64 .OR. uValue2:IsInt64
			RETURN (USUAL) Math.Min( (INT64) uValue1, (INT64) uValue2)
		ENDIF
		RETURN (USUAL) Math.Min( (LONG) uValue1, (LONG) uValue2)
	
	ELSEIF uValue1:IsDate .AND. uValue2:IsDate
		RETURN IIF ((DATE) uValue1 <(DATE) uValue2, uValue1, uValue2)
	
	ELSEIF uValue1:IsString .AND. uValue2:IsString
		RETURN IIF ((STRING) uValue1 <(STRING) uValue2, uValue1, uValue2)
	ELSE
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(uValue2) , "Incompatible types")
	ENDIF
	


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/palettergb/*" />
FUNCTION PaletteRGB(bR AS USUAL,bG AS USUAL,bB AS BYTE) AS INT
	return (INT) RGB(bR, bG, bB)
	

/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/pause/*" />
FUNCTION Pause() AS DWORD
    RETURN (DWORD) System.Windows.Forms.MessageBox.Show("Pause","Waiting")
	


     


/// <summary>
/// Install a system-wide object that receives all messages being sent to other data types.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
[Obsolete];
FUNCTION SysObject(o AS USUAL) AS OBJECT
	RETURN NULL_OBJECT   


/// <include file="VoFunctionDocs.xml" path="Runtimefunctions/tone/*" />
FUNCTION Tone(wFrequency AS DWORD,wDuration AS DWORD) AS USUAL
	Console.Beep( (INT)wFrequency, (INT)wDuration * 1000 / 18 )
RETURN	 NIL   


/// <summary>
/// Changes and/or reads a system setting. 
/// </summary>
/// <param name="nDefine">Is a positive integer identifying a system setting or system variable.
/// This should match the values from the Set enumerated type.</param>
/// <param name="newValue">The optional expression can specify a new value for a system setting.
/// The data type is dependent on the system setting designated by <paramref name="nDefine" />.</param>
/// <returns>When Set() is called without the argument <paramref name="newValue" /> ,
/// the function returns the current system setting designated by <paramref name="nDefine" /> .
/// If <paramref name="newValue" /> is specified, the corresponding system setting is set to <paramref name="newValue" />
/// and the value of the old setting is returned. 
/// </returns>
/// <seealso cref="T:XSharp.Set" />
/// <remarks>If you are coming from XHarbour or Xbase++ please don't use set.ch for the value of <paramref name="nDefine" />
/// because there are some differences between the values in this header file and the values used inside X#. </remarks>
FUNCTION Set(nDefine, newValue) AS USUAL CLIPPER
    LOCAL state AS XSharp.RuntimeState
    LOCAL oOld  := NULL AS OBJECT
    LOCAL nSetting AS LONG
    IF ! IsNumeric(nDefine)
        RETURN NIL
    ENDIF
    nSetting := nDefine
    state := XSharp.RuntimeState.GetInstance()
    IF state:Settings:ContainsKey(nSetting)
        oOld := state:Settings[nSetting]
    ENDIF
    IF PCount() > 1
        LOCAL oNew := newValue AS OBJECT
        IF oOld != NULL_OBJECT
            IF oOld IS LOGIC .and. oNew IS STRING VAR cNew
                // XBase++ uses ON / OFF strings in stead of TRUE and FALSE
                state:Settings[nSetting] := cNew:ToUpper() == "ON"
            ELSE
               TRY
                    oNew := System.Convert.ChangeType( oNew, oOld:GetType())
                    state:Settings[nSetting] := oNew
                CATCH
                    NOP // can't convert, so ignore assignment
                END TRY
            ENDIF
     
        ENDIF
    ENDIF
    RETURN oOld
            
    


