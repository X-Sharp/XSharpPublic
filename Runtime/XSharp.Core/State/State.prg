//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Threading
USING XSharp.RDD
USING XSharp.RDD.Enums

/// <summary>
/// Container Class that holds the XSharp Runtime state
/// </summary>
/// <remarks>
/// Please note that unlike in Visual Objects and Vulcan.NET every thread has its own copy of the runtime state.<br/>
/// The runtime state from a new thread is a copy of the state of the main thread at that moment.
/// </remarks>


CLASS XSharp.RuntimeState
	// Static Fields
	PRIVATE INITONLY STATIC initialState  AS RuntimeState 
	// Static Methods and Constructor
	private static currentState := ThreadLocal<RuntimeState>{ {=>  initialState:Clone()} }  as ThreadLocal<RuntimeState> 
	static constructor
		initialState	:= RuntimeState{true}
		
	/// <summary>Retrieve the runtime state for the current thread</summary>
	PUBLIC STATIC METHOD GetInstance() AS RuntimeState
		RETURN currentState:Value

	PRIVATE oSettings AS Dictionary<INT, OBJECT> 

	PRIVATE CONSTRUCTOR(initialize as logic)       
		var oThread := Thread.CurrentThread
		self:Name := "ThreadState for "+oThread:ManagedThreadId:ToString()
		oSettings := Dictionary<INT, OBJECT>{}
		if initialize
			self:BreakLevel := 0 
			self:_SetThreadValue(Set.DateFormat ,"MM/DD/YYYY")
			self:_SetThreadValue(Set.Epoch, 1900U)
			self:_SetThreadValue(Set.EpochYear, 0U)
			self:_SetThreadValue(Set.EpochCent, 1900U)
			// Initialize the values that are not 'blank'
			// RDD Settings
			self:_SetThreadValue(Set.Ansi , true)
			self:_SetThreadValue(Set.AutoOpen , true)
			self:_SetThreadValue(Set.AutoOrder , true)
			self:_SetThreadValue(Set.Optimize , true)
			self:_SetThreadValue(Set.AutoShare, AutoShareMode.Auto)
			self:_SetThreadValue(Set.LOCKTRIES , 1)
			self:_SetThreadValue(Set.MemoBlockSize , 32)
			self:_SetThreadValue(Set.DefaultRDD , "DBFNTX")
			self:_SetThreadValue(Set.Exclusive , TRUE)
			// Console Settings
			self:_SetThreadValue(Set.Bell , TRUE)
			self:_SetThreadValue(Set.Color , "W/N,N/W,N/N,N/N,N/W")
			self:_SetThreadValue(Set.Decimals , 2)
			self:_SetThreadValue(Set.Digits , 10)
			self:_SetThreadValue(Set.Exact , TRUE)
			self:_SetThreadValue(Set.FLoatDelta , 0.0000000000001)
			SELF:_SetThreadValue(Set.DOSCODEPAGE, Win32.GetDosCodePage())
			self:_SetThreadValue(Set.WINCODEPAGE, Win32.GetWinCodePage())
			// Add null value for Clipper collation 
			SELF:_SetThreadValue<Byte[]>(Set.CollationTable, null )

			// Date and time settings
			SELF:_SetInternationalWindows()

			// Other settings
			// Collation Table including Compare delegates
			// Nation DLL
			// Macro Compiler
			// WinCodePage ?
			// DosCodePage ?
			// LCID
			// SetDateFormat should also set DateCountry
			// Empty and Internal Dateformat strings 


		ENDIF
		RETURN

	DESTRUCTOR()
		// What do we need to clean ?
		if oSettings != null
			oSettings:Clear()
		endif

	PRIVATE METHOD Clone() AS RuntimeState
		LOCAL oNew AS RuntimeState
		oNew := RuntimeState{false}		
		BEGIN LOCK oSettings
			// Copy all values from Current State to New state
			FOREACH VAR element IN oSettings     
				oNew:oSettings[element:Key] := element:Value
			NEXT
		END LOCK
		RETURN oNew     
		
	/// <summary>Retrieve state name</summary>
	/// <returns>String value, such as "State for Thread 123"</returns>
	PUBLIC PROPERTY Name AS STRING AUTO
	/// <summary>Current Break Level. Gets set by compiler generated code for BEGIN SEQUENCE .. END constructs.</summary>
	PUBLIC PROPERTY BreakLevel as INT AUTO
	/// <summary>ToString() override</summary>
	/// <returns>String value, such as "State for Thread 123"</returns>
	PUBLIC VIRTUAL METHOD ToString() AS STRING
		RETURN SELF:Name

	/// <summary>Retrieve a value from the state of the current Thread.</summary>
	/// <param name="nSetting">Setting number to retrieve. Must be defined in the SET enum.</param>
	/// <typeparam name="T">The return type expected for this setting.</typeparam>
	/// <returns>The current value, or a default value of type T.</returns>
	PUBLIC STATIC METHOD GetValue<T> (nSetting AS INT) AS T
		RETURN currentState:Value:_GetThreadValue<T>(nSetting);

	/// <summary>Set a value for the state of the current Thread.</summary>
	/// <param name="nSetting">Setting number to retrieve. Must be defined in the SET enum.</param>
	/// <param name="oValue">The new value for the setting.</param>
	/// <typeparam name="T">The return type expected for this setting.</typeparam>
	/// <returns>The previous value, or a default value of type T when the setting was not yetr defined.</returns>
	PUBLIC STATIC METHOD SetValue<T> (nSetting AS INT, oValue AS T) AS T
		RETURN currentState:Value:_SetThreadValue<T>(nSetting, oValue)

	PRIVATE METHOD _GetThreadValue<T> (nSetting AS INT) AS T
		BEGIN LOCK oSettings
			IF oSettings.ContainsKey(nSetting)
				RETURN (T) oSettings[nSetting]
			ENDIF
		END LOCK
		RETURN Default(T)

	PRIVATE METHOD _SetThreadValue<T>(nSetting AS INT, oValue AS T) AS T
		LOCAL result AS T
		BEGIN LOCK oSettings
			IF oSettings.ContainsKey(nSetting)
				result := (T) oSettings[nSetting]
			ELSE
				result := Default(T)
			ENDIF
			oSettings[nSetting] := oValue
		END LOCK
		RETURN	result		

	#region properties from the Vulcan RuntimeState that are emulated

	/// <summary>The current compiler setting for the VO11 compiler option as defined when compiling the main application.
	/// This value gets assigned in the startup code for applications in the VO or Vulcan dialect.</summary>
	static property CompilerOptionVO11 as logic ;
        GET GetValue<LOGIC>(Set.OPTIONVO11);
        SET SetValue<LOGIC>(Set.OPTIONVO11, VALUE)

	/// <summary>The current compiler setting for the OVF compiler option as defined when compiling the main application.
	/// This value gets assigned in the startup code for applications in the VO or Vulcan dialect.</summary>
	static property CompilerOptionOVF as logic ;
        GET GetValue<LOGIC>(Set.OPTIONOVF);
        SET SetValue<LOGIC>(Set.OPTIONOVF, VALUE)

	/// <summary>The current compiler setting for the FOVF compiler option as defined when compiling the main application.
	/// This value gets assigned in the startup code for applications in the VO or Vulcan dialect.</summary>
	static property CompilerOptionFOVF as logic ;
        GET GetValue<LOGIC>(Set.OPTIONOVF);
        SET SetValue<LOGIC>(Set.OPTIONOVF, VALUE)

	/// <summary>The System.Reflection.Module for the main application.
	/// This value gets assigned in the startup code for applications in the VO or Vulcan dialect.</summary>
    STATIC PROPERTY AppModule AS  System.Reflection.Module;
        GET GetValue<System.Reflection.Module>(Set.AppModule);
        set SetValue<System.Reflection.Module>(Set.AppModule, value)
	#endregion


	/// <summary>The current ANSI setting</summary>
    STATIC PROPERTY @@Ansi AS LOGIC ;
        GET GetValue<LOGIC>(Set.Ansi);
        SET SetValue<LOGIC>(Set.Ansi, VALUE)

	/// <summary>The current AutoOrder setting (used by the RDD system).</summary>
    STATIC PROPERTY AutoOrder AS LOGIC ;
        GET GetValue<LOGIC>(Set.AutoOrder);
        SET SetValue<LOGIC>(Set.AutoOrder, VALUE)

	/// <summary>The current AutoOpen setting (used by the RDD system).</summary>
    STATIC PROPERTY AutoOpen AS LOGIC ;
        GET GetValue<LOGIC>(Set.AutoOpen);
        SET SetValue<LOGIC>(Set.AutoOpen, VALUE)

	/// <summary>The current AutoShareMode setting (used by the RDD system).</summary>
    STATIC PROPERTY AutoShareMode AS AutoShareMode ;
        GET GetValue<AutoShareMode>(Set.AutoShare);
        SET SetValue<AutoShareMode>(Set.AutoShare, VALUE)


	/// <summary>The current Century setting (used in DATE &lt;-&gt; STRING conversions).</summary>
   STATIC PROPERTY Century AS LOGIC ;
        GET GetValue<LOGIC>(Set.Century);
        SET SetValue<LOGIC>(Set.Century, VALUE)

	/// <summary>The current Collation mode (used by the RDD system).</summary>
   STATIC PROPERTY CollationMode AS CollationMode ;
        GET GetValue<CollationMode>(Set.CollationMode);
        SET SetValue<CollationMode>(Set.CollationMode, VALUE)

	/// <summary>The current DateCountry setting mode (used in DATE &lt;-&gt; STRING conversions).</summary>
   STATIC PROPERTY DateCountry AS INT ;
        GET GetValue<INT>(Set.DateCountry);
        SET SetValue<INT>(Set.DateCountry, VALUE)

	/// <summary>The current Date format</summary>
	/// <remarks>This string should contain a combination of DD MM and either YY or YYYY characters.<br/>
	/// For example DD-MM-YYYY for italian date format, MM/DD/YYYY for American date format or DD/MM/YYYY for British Date format.
	/// Note that all other characters except the four groups mentioned above are copied to the output string verbatim.
	/// </remarks>
    STATIC PROPERTY DateFormat AS STRING ;
        GET GetValue<STRING>(Set.DateFormat);
        SET _SetDateFormat(Value)

	/// <summary>The default number of decimals for new FLOAT values that are created without explicit decimals</summary>

    STATIC PROPERTY Decimals AS LONG ;
        GET GetValue<LONG>(Set.DECIMALS);
        SET SetValue<LONG>(Set.DECIMALS, VALUE)

	/// <summary>The default number of decimals for new FLOAT values that are created without explicit decimals</summary>
    STATIC PROPERTY DecimalSep AS WORD ;
        GET GetValue<WORD>(Set.DecimalSep);
        SET SetValue<WORD>(Set.DecimalSep, VALUE)

	/// <summary>RDD Deleted Flag that determines whether to ignore or include records that are marked for deletion.</summary>
    STATIC PROPERTY Deleted AS LOGIC ;
        GET GetValue<LOGIC>(Set.DELETED);
        SET SetValue<LOGIC>(Set.DELETED, VALUE)

	/// <summary>The default number of digits for new FLOAT values that are created without explicit decimals</summary>
    STATIC PROPERTY Digits AS LONG ;
        GET GetValue<LONG>(Set.DIGITS);
        SET SetValue<LONG>(Set.DIGITS, VALUE)

	/// <summary>The DOS Codepage. This gets read at startup from the OS().</summary>
    STATIC PROPERTY DosCodePage AS LONG ;
        GET GetValue<LONG>(Set.DOSCODEPAGE);
        SET SetValue<LONG>(Set.DOSCODEPAGE, VALUE)


	/// <summary>Date Epoch value that determines how dates without century digits are interpreted.</summary>
    STATIC PROPERTY Epoch AS DWORD ;
        GET GetValue<DWORD>(Set.EPOCH);
        SET SetValue<DWORD>(Set.EPOCH, VALUE)

	/// <summary>Date Epoch Year value. This gets set by the SetEpoch() function to the Epoch year % 100.</summary>
    STATIC PROPERTY EpochYear AS DWORD ;
        GET GetValue<DWORD>(Set.EPOCHYEar)

	/// <summary>Date Epoch Century value. This gets set by the SetEpoch() function to the century in which the Epoch year falls.</summary>
    STATIC PROPERTY EpochCent AS DWORD ;
        GET GetValue<DWORD>(Set.EPOCHCent)


	/// <summary>String comparison Exact flag that determines how comparisons with the single '=' characters should be done.</summary>
    STATIC PROPERTY Exact AS LOGIC ;
        GET GetValue<LOGIC>(Set.EXACT);
        SET SetValue<LOGIC>(Set.EXACT, VALUE)

	/// <summary>Numeric value that controls the precision of Float comparisons.</summary>
   STATIC PROPERTY FloatDelta AS Real8 ;
        GET GetValue<Real8>(Set.FloatDelta);
        SET SetValue<Real8>(Set.FloatDelta, VALUE)

	/// <summary>Current SetInternational Setting.</summary>
     STATIC PROPERTY International AS CollationMode ;
        GET GetValue<CollationMode>(Set.Intl);
        SET SetValue<CollationMode>(Set.Intl, VALUE)

	/// <summary>Number of tries that were done when the last lock operation failed.</summary>
    STATIC PROPERTY LockTries AS LONG ;
        GET GetValue<LONG>(Set.LOCKTRIES);
        SET SetValue<LONG>(Set.LOCKTRIES, VALUE)

	/// <summary>The current default MemoBlock size.</summary>
    STATIC PROPERTY MemoBlockSize AS LONG ;
        GET GetValue<LONG>(Set.MEMOBLOCKSIZE);
        SET SetValue<LONG>(Set.MEMOBLOCKSIZE, VALUE)


	/// <summary>Did the last RDD operation cause a Network Error ?</summary>
    STATIC PROPERTY NetErr AS LOGIC;
        GET GetValue<LOGIC>(Set.NETERR);
        SET SetValue<LOGIC>(Set.NETERR, VALUE)

	/// <summary>RDD Optimize Flag</summary>
    STATIC PROPERTY Optimize AS LOGIC ;
        GET GetValue<LOGIC>(Set.OPTIMIZE);
        SET SetValue<LOGIC>(Set.OPTIMIZE, VALUE)

	/// <summary>The current SetSoftSeek flag.</summary>
    STATIC PROPERTY SoftSeek AS LOGIC ;
        GET GetValue<LOGIC>(Set.SOFTSEEK);
        SET SetValue<LOGIC>(Set.SOFTSEEK, VALUE)

	/// <summary>The Thousand separator</summary>
    STATIC PROPERTY ThousandSep AS WORD ;
        GET GetValue<WORD>(Set.THOUSANDSEP);
        SET SetValue<WORD>(Set.THOUSANDSEP, VALUE)


	/// <summary>Number of tries that were done when the last lock operation failed.</summary>
    STATIC PROPERTY Unique AS LOGIC ;
        GET GetValue<LOGIC>(Set.UNIQUE);
        SET SetValue<LOGIC>(Set.UNIQUE, VALUE)

	/// <summary>The Windows Codepage. This gets read at startup from the OS().</summary>
    STATIC PROPERTY WinCodePage AS LONG ;
        GET GetValue<LONG>(Set.WINCODEPAGE);
        SET SetValue<LONG>(Set.WINCODEPAGE, VALUE)


	/// <summary>The name of the method that was called in the last late bound method call.</summary>
    STATIC PROPERTY NoMethod AS STRING ;
        GET GetValue<STRING>(Set.NoMethod);
        SET SetValue<STRING>(Set.NoMethod, VALUE)



//	STATIC METHOD SetInternational(mode AS CollationMode, force := FALSE AS LOGIC) AS VOID
//		IF mode != RuntimeState.International	.or. force
//			if mode == CollationMode.Clipper
//				currentState:Value:_SetInternationalClipper()				
//			ELSE
//				currentState:Value:_SetInternationalWindows()				
//			ENDIF
//		ENDIF
	internal method _SetInternationalClipper() as void
		self:_SetThreadValue(Set.AMEXT, "")
		self:_SetThreadValue(Set.PMEXT, "")
		self:_SetThreadValue(Set.AMPM, FALSE)
		self:_SetThreadValue(Set.Century, FALSE)
		self:_SetThreadValue(Set.DateCountry, 1)
		self:_SetThreadValue(Set.Decimals, 2)
		self:_SetThreadValue(Set.DECIMALSEP, (word) 46)		// DOT .
		self:_SetThreadValue(Set.THOUSANDSEP, (word) 44)	// COMMA ,
		self:_SetThreadValue(Set.DateFormat, "MM/DD/YYYY")
		self:_SetThreadValue(Set.Intl, CollationMode.Clipper)

	internal method _SetInternationalWindows() as void
		VAR dtInfo	    := System.Globalization.DateTimeFormatInfo.CurrentInfo
		self:_SetThreadValue(Set.AMEXT, dtInfo:AMDesignator)
		self:_SetThreadValue(Set.PMEXT, dtInfo:PMDesignator)
		var separator := dtInfo:TimeSeparator
		if String.IsNullOrEmpty(separator)
			self:_SetThreadValue(Set.TimeSep, (word) 0)
		else
			self:_SetThreadValue(Set.TimeSep, (word) separator[0])
		endif
		self:_SetThreadValue(Set.AMPM, dtInfo:ShortDatePattern:IndexOf("tt") != -1)
		VAR dateformat  := dtInfo:ShortDatePattern:ToLower()
		// reduce to single m and d
		if (dateformat.IndexOf("mm") != -1)
			dateformat		:= dateformat:Replace("mmmm", "m")
			dateformat		:= dateformat:Replace("mmm", "m")
			dateformat		:= dateformat:Replace("mm", "m")
		ENDIF
		IF dateformat.IndexOf("dd") != -1
			dateformat		:= dateformat:Replace("dd", "d")
		ENDIF
		_SetThreadValue(Set.Century, dateformat:IndexOf("yyyy") != -1)
		_SetThreadValue(Set.DateFormatNet, dateformat:ToUpper():Replace("D","d"):Replace("Y","y"):Replace("/","'/'"))
		_SetThreadValue(Set.DateFormatEmpty, dateformat:ToUpper():Replace("D"," "):Replace("Y"," "):Replace("M"," "))
		dateformat := dateformat:Replace("d", "dd"):Replace("m","mm"):ToUpper()
		self:_SetThreadValue(Set.DateFormat,  dateformat)
		self:_SetThreadValue(Set.DateCountry, 1)
		self:_SetThreadValue(Set.DECIMALS , 2)
		VAR numberformat := System.Globalization.NumberFormatInfo.CurrentInfo
		self:_SetThreadValue(Set.DECIMALSEP, (Word) numberformat:NumberDecimalSeparator[0])
		self:_SetThreadValue(Set.THOUSANDSEP, (word) numberformat:NumberGroupSeparator[0])
		self:_SetThreadValue(Set.EPOCH, 1910U)
		self:_SetThreadValue(Set.EpochYear, 10U)
		self:_SetThreadValue(Set.EpochCent, 1900U)

		self:_SetThreadValue(Set.Intl, CollationMode.Windows)
		RETURN

	INTERNAL STATIC METHOD _SetDateFormat(format AS STRING) AS VOID
		format := format:ToUpper()
		// ensure we have dd, mm and yy
		if format:IndexOf("DD") == -1 .or. format:IndexOf("MM") == -1 .or. format:IndexOf("YY") == -1
			return
		endif
		SetValue(Set.DateFormatNet, format:Replace("D","d"):Replace("Y","y"):Replace("/","'/'"))
		SetValue(Set.DateFormatEmpty, format:Replace("D"," "):Replace("Y"," "):Replace("M"," "))
		SetValue(SET.CENTURY, format:Contains("YYYY"))
		SetValue(Set.DATEFORMAT, format)
		SWITCH format
			CASE "MM/DD/YY"
			CASE "MM/DD/YYYY"
				SetValue(Set.DATECOUNTRY, 1)	// American
			CASE "YY.MM.DD"
			CASE "YYYY.MM.DD"
				SetValue(Set.DATECOUNTRY, 2)	// Ansi
			CASE "DD/MM/YY"
			CASE "DD/MM/YYYY"
				SetValue(Set.DATECOUNTRY, 3)	// British & french
			CASE "DD.MM.YY"
			CASE "DD.MM.YYYY"
				SetValue(Set.DATECOUNTRY, 5)	// German
			CASE "DD-MM-YY"
			CASE "DD-MM-YYYY"
				SetValue(Set.DATECOUNTRY, 6)	// Italian
			CASE "YY/MM/DD"
			CASE "YYYY/MM/DD"
				SetValue(Set.DATECOUNTRY, 7)	// Japanese
			CASE "MM-DD-YY"
			CASE "MM-DD-YYYY"
				SetValue(Set.DATECOUNTRY, 8)	// USA
			OTHERWISE
				SetValue(Set.DATECOUNTRY, 0)	
		END SWITCH

	private _workareas as WorkAreas
	/// <summary>The workarea information for the current Thread.</summary>
	public property Workareas as WorkAreas
	get
		if _workareas == null_object
			_workareas := WorkAreas{}
		endif
		return _workareas
	end get
	END PROPERTY

	STATIC PRIVATE _macrocompiler AS System.Type
	public STATIC property MacroCompiler as System.Type GET _macrocompiler SET _macrocompiler := Value

END CLASS




