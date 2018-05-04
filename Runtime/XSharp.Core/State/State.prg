//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Threading
USING XSharp.RDD

/// <Summary>
/// Container Class that holds the XSharp Runtime state
/// </Summary>
/// <Remarks>
/// Please note that unlike in Visual Objects and Vulcan.NET every thread has 
/// its own copy of the runtime state.</br>
/// The runtime state from a new thread is a copy of the state of the main thread at that moment.
/// </Remarks>


CLASS XSharp.RuntimeState
	// Static Fields
	PRIVATE INITONLY STATIC initialState  AS RuntimeState 
	// Static Methods and Constructor
	private static currentState := ThreadLocal<RuntimeState>{ {=>  initialState:Clone()} }  as ThreadLocal<RuntimeState> 
	static constructor
		initialState	:= RuntimeState{true}
		
	/// <Summary>Retrieve the runtime state for the current thread</Summary>
	PUBLIC STATIC METHOD GetInstance() AS RuntimeState
		RETURN currentState:Value

	/// <Summary>List of number - value pairs </Summary>
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
		
	/// <Summary>Retrieve state name</Summary>
	/// <Returns>String value, such as "State for Thread 123"</Returns>
	PUBLIC PROPERTY Name AS STRING AUTO
	PUBLIC PROPERTY BreakLevel as INT AUTO
	/// <Summary>ToString() override</Summary>
	/// <Returns>String value, such as "State for Thread 123"</Returns>
	PUBLIC VIRTUAL METHOD ToString() AS STRING
		RETURN SELF:Name

	PUBLIC STATIC METHOD GetValue<T> (nSetting AS INT) AS T
		RETURN currentState:Value:_GetThreadValue<T>(nSetting);

	PUBLIC STATIC METHOD SetValue<T> (nSetting AS INT, oValue AS T) AS T
		RETURN currentState:Value:_SetThreadValue<T>(nSetting, oValue)

	/// <Summary>Get the value for a certain setting</Summary>
	/// <param Name="nSetting"> The number of the setting to change</param>
	/// <typeparam Name="T"> The expected return type of the value</typeparam>
	/// <Returns>The new value</Returns>
	PRIVATE METHOD _GetThreadValue<T> (nSetting AS INT) AS T
		BEGIN LOCK oSettings
			IF oSettings.ContainsKey(nSetting)
				RETURN (T) oSettings[nSetting]
			ENDIF
		END LOCK
		RETURN Default(T)

	/// <Summary>Set the value for a certain setting</Summary>
	/// <param Name="nSetting"> The number of the setting to change</param>
	/// <param Name="oValue"> The new value of the setting.</param>
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

	static property CompilerOptionVO11 as logic ;
        GET GetValue<LOGIC>(Set.OPTIONVO11);
        SET SetValue<LOGIC>(Set.OPTIONVO11, VALUE)

	static property CompilerOptionOVF as logic ;
        GET GetValue<LOGIC>(Set.OPTIONOVF);
        SET SetValue<LOGIC>(Set.OPTIONOVF, VALUE)
	
	static property CompilerOptionFOVF as logic ;
        GET GetValue<LOGIC>(Set.OPTIONOVF);
        SET SetValue<LOGIC>(Set.OPTIONOVF, VALUE)

    STATIC PROPERTY AppModule AS  System.Reflection.Module;
        GET GetValue<System.Reflection.Module>(Set.AppModule);
        set SetValue<System.Reflection.Module>(Set.AppModule, value)
	#endregion

    STATIC PROPERTY @@Ansi AS LOGIC ;
        GET GetValue<LOGIC>(Set.Ansi);
        SET SetValue<LOGIC>(Set.Ansi, VALUE)



    STATIC PROPERTY AutoOrder AS LOGIC ;
        GET GetValue<LOGIC>(Set.AutoOrder);
        SET SetValue<LOGIC>(Set.AutoOrder, VALUE)

    STATIC PROPERTY AutoOpen AS LOGIC ;
        GET GetValue<LOGIC>(Set.AutoOpen);
        SET SetValue<LOGIC>(Set.AutoOpen, VALUE)

    STATIC PROPERTY AutoShareMode AS AutoShareMode ;
        GET GetValue<AutoShareMode>(Set.AutoShare);
        SET SetValue<AutoShareMode>(Set.AutoShare, VALUE)

   STATIC PROPERTY Century AS LOGIC ;
        GET GetValue<LOGIC>(Set.Century);
        SET SetValue<LOGIC>(Set.Century, VALUE)

   STATIC PROPERTY CollationMode AS CollationMode ;
        GET GetValue<CollationMode>(Set.CollationMode);
        SET SetValue<CollationMode>(Set.CollationMode, VALUE)

   STATIC PROPERTY DateCountry AS INT ;
        GET GetValue<INT>(Set.DateCountry);
        SET SetValue<INT>(Set.DateCountry, VALUE)

	/// <Summary>The current Date format</Summary>
	/// <Remarks>This string should contain a combination of DD MM and either YY or YYYY characters.<br>
	/// For example DD-MM-YYYY for italian date format, MM/DD/YYYY for American date format or DD/MM/YYYY for British Date format.
	/// Note that all other characters except the four groups mentioned above are copied to the output string verbatim.
	/// </Remarks>
	/// <Returns>String value</Returns>
    STATIC PROPERTY DateFormat AS STRING ;
        GET GetValue<STRING>(Set.DateFormat);
        SET _SetDateFormat(Value)

	/// <Summary>The default number of decimals for new FLOAT values that are created without explicit decimals</Summary>
	/// <Returns>DWORD value</Returns>
    STATIC PROPERTY Decimals AS LONG ;
        GET GetValue<LONG>(Set.DECIMALS);
        SET SetValue<LONG>(Set.DECIMALS, VALUE)

	/// <Summary>The default number of decimals for new FLOAT values that are created without explicit decimals</Summary>
	/// <Returns>DWORD value</Returns>
    STATIC PROPERTY DecimalSep AS WORD ;
        GET GetValue<WORD>(Set.DecimalSep);
        SET SetValue<WORD>(Set.DecimalSep, VALUE)

	/// <Summary>RDD Deleted Flag that determines whether to ignore or include records that are marked for deletion.</Summary>
	/// <Returns>Logic value</Returns>
    STATIC PROPERTY Deleted AS LOGIC ;
        GET GetValue<LOGIC>(Set.DELETED);
        SET SetValue<LOGIC>(Set.DELETED, VALUE)

	/// <Summary>The default number of digits for new FLOAT values that are created without explicit decimals</Summary>
	/// <Returns>DWORD value</Returns>
    STATIC PROPERTY Digits AS LONG ;
        GET GetValue<LONG>(Set.DIGITS);
        SET SetValue<LONG>(Set.DIGITS, VALUE)

	/// <Summary>Date Epoch value that determines how dates without century digits are interpreted.</Summary>
	/// <Returns>DWORD value</Returns>
    STATIC PROPERTY Epoch AS DWORD ;
        GET GetValue<DWORD>(Set.EPOCH);
        SET SetValue<DWORD>(Set.EPOCH, VALUE)

    STATIC PROPERTY EpochYear AS DWORD ;
        GET GetValue<DWORD>(Set.EPOCHYEar)

    STATIC PROPERTY EpochCent AS DWORD ;
        GET GetValue<DWORD>(Set.EPOCHCent)


	/// <Summary>String comparison Exact flag that determines how comparisons with the single '=' characters should be done.</Summary>
	/// <Returns>Logic value</Returns>
    STATIC PROPERTY Exact AS LOGIC ;
        GET GetValue<LOGIC>(Set.EXACT);
        SET SetValue<LOGIC>(Set.EXACT, VALUE)

   STATIC PROPERTY FLoatDelta AS Real8 ;
        GET GetValue<Real8>(Set.FloatDelta);
        SET SetValue<Real8>(Set.FloatDelta, VALUE)

     STATIC PROPERTY International AS CollationMode ;
        GET GetValue<CollationMode>(Set.Intl);
        SET SetValue<CollationMode>(Set.Intl, VALUE)

    STATIC PROPERTY LockTries AS LONG ;
        GET GetValue<LONG>(Set.LOCKTRIES);
        SET SetValue<LONG>(Set.LOCKTRIES, VALUE)

    STATIC PROPERTY MemoBlockSize AS LONG ;
        GET GetValue<LONG>(Set.MEMOBLOCKSIZE);
        SET SetValue<LONG>(Set.MEMOBLOCKSIZE, VALUE)


    STATIC PROPERTY NetErr AS LOGIC;
        GET GetValue<LOGIC>(Set.NETERR);
        SET SetValue<LOGIC>(Set.NETERR, VALUE)
	/// <Summary>RDD Optimize Flag</Summary>
	/// <Returns>Logic value</Returns>
    STATIC PROPERTY Optimize AS LOGIC ;
        GET GetValue<LOGIC>(Set.OPTIMIZE);
        SET SetValue<LOGIC>(Set.OPTIMIZE, VALUE)

    STATIC PROPERTY SoftSeek AS LOGIC ;
        GET GetValue<LOGIC>(Set.SoftSeek);
        SET SetValue<LOGIC>(Set.SoftSeek, VALUE)

	/// <Summary>The default number of decimals for new FLOAT values that are created without explicit decimals</Summary>
	/// <Returns>DWORD value</Returns>
    STATIC PROPERTY ThousandSep AS WORD ;
        GET GetValue<WORD>(Set.ThousandSep);
        SET SetValue<WORD>(Set.ThousandSep, VALUE)


    STATIC PROPERTY Unique AS LOGIC ;
        GET GetValue<LOGIC>(Set.Unique);
        SET SetValue<LOGIC>(Set.Unique, VALUE)

    STATIC PROPERTY NoMethod AS STRING ;
        GET GetValue<STRING>(Set.NoMethod);
        SET SetValue<STRING>(Set.NoMethod, VALUE)



	STATIC METHOD SetInternational(mode AS CollationMode, force := FALSE AS LOGIC) AS VOID
		IF mode != RuntimeState.International	.or. force
			if mode == CollationMode.Clipper
				currentState:Value:_SetInternationalClipper()				
			ELSE
				currentState:Value:_SetInternationalWindows()				
			ENDIF
		ENDIF
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
	public property Workareas as WorkAreas
	get
		if _workareas == null_object
			_workareas := WorkAreas{}
		endif
		return _workareas
	end get
	end property
END CLASS




