//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//



/// <summary>
/// Determine if a value is between two other values.
/// </summary>
/// <param name="x">Value which should be compared.</param>
/// <param name="y">Lower value to compare against.</param>
/// <param name="z">Upper value to compare against.</param>
/// <returns>
/// True if x is &gt;= y and &lt;= z otherwise false.
/// </returns>
FUNCTION Between(val AS USUAL,min AS USUAL,max AS USUAL) AS LOGIC
	RETURN val >=min .AND.  val<=max




/// <summary>
/// Indicate whether the first expression in a series is repeated later in the series.
/// </summary>
/// <param name="u"></param>
/// <returns>
/// </returns>
FUNCTION InList(u AS USUAL, args PARAMS USUAL[]) AS LOGIC
	RETURN _InListWorker(u, args, FALSE)
/// <summary>
/// Indicate whether the first expression in a series is repeated in the exact same form later in the series.
/// </summary>
/// <param name="u"></param>
/// <returns>
/// </returns>
FUNCTION InListExact(u AS USUAL, args PARAMS USUAL[]) AS LOGIC
	RETURN _InListWorker(u, args, TRUE)


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




/// <summary>
/// Return the larger of 2 values.
/// </summary>
/// <param name="u1"></param>
/// <param name="u2"></param>
/// <returns>
/// </returns>
FUNCTION Max(u1 AS USUAL,u2 AS USUAL) AS USUAL

	IF u1:IsNumeric .AND. u2:IsNumeric

		IF u1:IsFloat .OR. u2:IsFloat
			RETURN (USUAL) Math.Max( (REAL8) u1, (REAL8) u2)

		ELSEIF u1:IsDecimal .OR. u2:IsDecimal
			RETURN (USUAL) Math.Max( (Decimal) u1, (Decimal) u2)

		ELSEIF u1:IsInt64 .OR. u2:IsInt64
			RETURN (USUAL) Math.Max( (INT64) u1, (INT64) u2)
		ENDIF
		RETURN (USUAL) Math.Max( (LONG) u1, (LONG) u2)

	ELSEIF u1:IsDate .AND. u2:IsDate
		RETURN IIF ((DATE) u1 > (DATE) u2, u1, u2)

	ELSEIF u1:IsDateTime .AND. u2:IsDateTime
		RETURN IIF ((DateTime) u1 > (DateTime) u2, u1, u2)

	ELSEIF (u1:IsDateTime .OR. u1:IsDate) .AND. (u2:IsDateTime .OR. u2:IsDate)
		RETURN IIF ((DateTime) u1 > (DateTime) u2, u1, u2)

	ELSEIF u1:IsString .AND. u2:IsString
		RETURN IIF ((STRING) u1 > (STRING) u2, u1, u2)

	ELSEIF u1:IsSymbol .AND. u2:IsSymbol
		RETURN IIF ((SYMBOL) u1 > (SYMBOL) u2, u1, u2)

	ELSE
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(u2) , "Incompatible types")
	ENDIF
	



/// <summary>
/// Return the smaller of 2 values.
/// </summary>
/// <param name="u1"></param>
/// <param name="u2"></param>
/// <returns>
/// </returns>
FUNCTION Min(u1 AS USUAL,u2 AS USUAL) AS USUAL
	IF u1:IsNumeric .AND. u2:IsNumeric

		IF u1:IsFloat .OR. u2:IsFloat
			
			RETURN (USUAL) Math.Min((REAL8) u1, (REAL8) u2)
		
		ELSEIF u1:IsDecimal .OR. u2:IsDecimal
			RETURN (USUAL) Math.Min( (Decimal) u1, (Decimal) u2)
		
		ELSEIF u1:IsInt64 .OR. u2:IsInt64
			RETURN (USUAL) Math.Min( (INT64) u1, (INT64) u2)
		ENDIF
		RETURN (USUAL) Math.Min( (LONG) u1, (LONG) u2)
	
	ELSEIF u1:IsDate .AND. u2:IsDate
		RETURN IIF ((DATE) u1 <(DATE) u2, u1, u2)
	
	ELSEIF u1:IsString .AND. u2:IsString
		RETURN IIF ((STRING) u1 <(STRING) u2, u1, u2)
	ELSE
        THROW Error.ArgumentError( __FUNCTION__, NAMEOF(u2) , "Incompatible types")
	ENDIF
	


/// <summary>This function is not implemented yet</summary>
// <summary>
// Get a particular color from a user-defined palette.
// </summary>
/// <param name="bR"></param>
/// <param name="bG"></param>
/// <param name="bB"></param>
/// <returns>
/// </returns>
FUNCTION PaletteRGB(bR AS USUAL,bG AS USUAL,bB AS BYTE) AS INT
	THROW NotImplementedException{}
	

/// <summary>This function is not implemented yet</summary>
// <summary>
// Display a system modal dialog box to pause the current application.
// </summary>
/// <returns>
/// </returns>
FUNCTION Pause() AS DWORD
	THROW NotImplementedException{}
	





/// <summary>
/// Install a system-wide object that receives all messages being sent to other data types.
/// </summary>
/// <param name="o"></param>
/// <returns>
/// </returns>
[Obsolete];
FUNCTION SysObject(o AS USUAL) AS OBJECT
	RETURN NULL_OBJECT   


/// <summary>
/// Sound a speaker tone for a specified frequency and duration.
/// </summary>
/// <param name="dwFreq"></param>
/// <param name="dwDur"></param>
/// <returns>
/// </returns>
FUNCTION Tone(dwFreq AS DWORD,dwDur AS DWORD) AS USUAL
	Console.Beep( (INT)dwFreq, (INT)dwDur * 1000 / 18 )
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
    LOCAL old  := NULL AS OBJECT
    LOCAL nSetting AS LONG
    IF ! IsNumeric(nDefine)
        RETURN NIL
    ENDIF
    nSetting := nDefine
    state := XSharp.RuntimeState.GetInstance()
    IF state:Settings:ContainsKey(nSetting)
        old := state:Settings[nSetting]
    ENDIF
    IF PCount() > 1
        LOCAL oValue := newValue AS OBJECT
        IF old != NULL_OBJECT
            TRY
                oValue := System.Convert.ChangeType( oValue, old:GetType())
                state:Settings[nSetting] := oValue
            CATCH
                NOP // can't convert, so ignore assignment
            END TRY
        ENDIF
    ENDIF
    RETURN old
            
    


