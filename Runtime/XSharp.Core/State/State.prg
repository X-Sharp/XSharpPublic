//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Threading
USING XSharp.RDD
USING XSharp.RDD.Enums
USING System.Runtime.CompilerServices
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
	private STATIC currentState := ThreadLocal<RuntimeState>{ {=>  initialState:Clone()},TRUE }  AS ThreadLocal<RuntimeState> 
	STATIC CONSTRUCTOR
		initialState	:= RuntimeState{TRUE}
		
	/// <summary>Retrieve the runtime state for the current thread</summary>
	PUBLIC STATIC METHOD GetInstance() AS RuntimeState
		RETURN currentState:Value

	PRIVATE oSettings AS Dictionary<INT, OBJECT>
    /// <summary>The dictionary that stores most of the settings in the runtime state. The key to the index is the number from the Set Enum</summary>
    /// <seealso cref="T:XSharp.Set" >Set Enum</seealso>
    PUBLIC PROPERTY Settings AS Dictionary<INT, OBJECT> GET oSettings

	PRIVATE CONSTRUCTOR(initialize AS LOGIC)       
		VAR oThread := Thread.CurrentThread
		SELF:Name := "ThreadState for "+oThread:ManagedThreadId:ToString()
		oSettings := Dictionary<INT, OBJECT>{}
		IF initialize
			SELF:BreakLevel := 0 
			SELF:_SetThreadValue(Set.DateFormat ,"MM/DD/YYYY")
			SELF:_SetThreadValue(Set.Epoch, 1900U)
			SELF:_SetThreadValue(Set.EpochYear, 0U)
			SELF:_SetThreadValue(Set.EpochCent, 1900U)
			// Initialize the values that are not 'blank'
			// RDD Settings
			SELF:_SetThreadValue(Set.Ansi , TRUE)
            SELF:_SetThreadValue(Set.Charset, 0)        // 0 = Ansi, 1 = Oem
			SELF:_SetThreadValue(Set.AutoOpen , TRUE)
			SELF:_SetThreadValue(Set.AutoOrder , 1)
			SELF:_SetThreadValue(Set.Optimize , TRUE)
            SELF:_SetThreadValue(Set.Deleted , FALSE)
			SELF:_SetThreadValue(Set.AutoShare, (LONG) AutoShareMode.Auto)
			SELF:_SetThreadValue(Set.LockTries , 1U)
			SELF:_SetThreadValue(Set.MemoBlockSize , 32U)
			SELF:_SetThreadValue(Set.DefaultRDD , "DBFNTX")
			SELF:_SetThreadValue(Set.Exclusive , TRUE)
            SELF:_SetThreadValue(Set.FoxLock , FALSE)
			// Console Settings
			SELF:_SetThreadValue(Set.Bell , TRUE)
			SELF:_SetThreadValue(Set.Color , "W/N,N/W,N/N,N/N,N/W")
			SELF:_SetThreadValue(Set.Decimals , (DWORD) 2)
			SELF:_SetThreadValue(Set.Digits , (DWORD) 10 )
			SELF:_SetThreadValue(Set.Exact , FALSE)
			SELF:_SetThreadValue(Set.FloatDelta , 0.0000000000001)
            SELF:_SetThreadValue<BYTE[]>(Set.CollationTable, NULL )
			IF System.Environment.OSVersion:Platform == System.PlatformID.Win32NT
                SELF:_SetThreadValue(Set.DOSCODEPAGE, Win32.GetDosCodePage())
                SELF:_SetThreadValue(Set.WINCODEPAGE, Win32.GetWinCodePage())
                SELF:_SetThreadValue(Set.CollationMode, CollationMode.Windows)
            ELSE
                SELF:_SetThreadValue(Set.DOSCODEPAGE, 437 )
                SELF:_SetThreadValue(Set.WINCODEPAGE, 1250 )
                SELF:_SetThreadValue(Set.CollationMode, CollationMode.Unicode )
            ENDIF
            SELF:_SetThreadValue(Set.Dialect, XSharpDialect.Core)
			// Add null value for Clipper collation 
			SELF:_SetThreadValue(Set.CollationMode, CollationMode.Windows)
			// Date and time settings
			SELF:_SetInternationalWindows()


		ENDIF
		RETURN
    /// <exclude />
	DESTRUCTOR()
		// What do we need to clean ?
		IF oSettings != NULL
			oSettings:Clear()
		ENDIF

    /// <summary>This method closes all open workareas for all threads. It is automatically called ad shutdown.</summary>
    /// <remarks>It is usually better to manage the lifetime of your workarea yourself in code, so don't trust on the runtime to close the workareas.</remarks>
    STATIC METHOD CloseWorkareasForAllThreads() AS VOID
        TRY
            FOREACH VAR state IN currentState:Values
                IF state:_workareas != NULL
                    state:_workareas:CloseAll()
                ENDIF
            NEXT
        CATCH e AS Exception
            System.Diagnostics.Debug.WriteLine(e:ToString())
        END TRY
	PRIVATE METHOD Clone() AS RuntimeState
		LOCAL oNew AS RuntimeState
		oNew := RuntimeState{FALSE}		
		BEGIN LOCK oSettings
			// Copy all values from Current State to New state
			FOREACH VAR element IN oSettings     
				oNew:oSettings[element:Key] := element:Value
			NEXT
		END LOCK
		RETURN oNew     
		
	/// <summary>Retrieve state name</summary>
	/// <returns>String value, such as "State for Thread 123"</returns>IDb
	PUBLIC PROPERTY Name AS STRING AUTO
	/// <summary>Current Break Level. Gets set by compiler generated code for BEGIN SEQUENCE .. END constructs.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
	PUBLIC PROPERTY BreakLevel AS INT AUTO
	/// <summary>ToString() override</summary>
	/// <returns>String value, such as "State for Thread 123"</returns>
	PUBLIC OVERRIDE METHOD ToString() AS STRING
		RETURN SELF:Name

	/// <summary>Retrieve a value from the state of the current Thread.</summary>
	/// <param name="nSetting">Setting number to retrieve. Must be defined in the SET enum.</param>
	/// <typeparam name="T">The return type expected for this setting.</typeparam>
	/// <returns>The current value, or a default value of type T.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
	PUBLIC STATIC METHOD GetValue<T> (nSetting AS INT) AS T
		RETURN currentState:Value:_GetThreadValue<T>(nSetting);

	/// <summary>Set a value for the state of the current Thread.</summary>
	/// <param name="nSetting">Setting number to retrieve. Must be defined in the SET enum.</param>
	/// <param name="oValue">The new value for the setting.</param>
	/// <typeparam name="T">The return type expected for this setting.</typeparam>
	/// <returns>The previous value, or a default value of type T when the setting was not yetr defined.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
	PUBLIC STATIC METHOD SetValue<T> (nSetting AS INT, oValue AS T) AS T
		RETURN currentState:Value:_SetThreadValue<T>(nSetting, oValue)
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
	PRIVATE METHOD _GetThreadValue<T> (nSetting AS INT) AS T
		BEGIN LOCK oSettings
            LOCAL result AS OBJECT
            IF oSettings:TryGetValue(nSetting, OUT result)
				RETURN (T) result
			ENDIF
		END LOCK
		RETURN DEFAULT(T) 
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
	PRIVATE METHOD _SetThreadValue<T>(nSetting AS INT, oValue AS T) AS T
		LOCAL result AS T
		BEGIN LOCK oSettings
            LOCAL oResult AS OBJECT
			IF oSettings:TryGetValue(nSetting, OUT oResult)
				result := (T) oResult
			ELSE
				result := DEFAULT(T)
			ENDIF
			oSettings[nSetting] := oValue
		END LOCK
		RETURN	result		 

	#region properties FROM the Vulcan RuntimeState that are emulated

	/// <summary>The current compiler setting for the VO11 compiler option.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default vale for this option is 'False'.</value>
	STATIC PROPERTY CompilerOptionVO11 AS LOGIC AUTO
 
	/// <summary>The current compiler setting for the VO13 compiler option.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default vale for this option is 'False'.</value>
	STATIC PROPERTY CompilerOptionVO13 AS LOGIC AUTO

	/// <summary>Gets / Sets the current workarea number.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY CurrentWorkArea AS DWORD ;
        GET Workareas:CurrentWorkAreaNO ;
        SET Workareas:CurrentWorkAreaNO  := VALUE

    /// <summary>The current compiler setting for the OVF compiler option.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default vale for this option is 'False'.</value>
	STATIC PROPERTY CompilerOptionOVF AS LOGIC AUTO

    /// <summary>The current compiler setting for the X# Dialect.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default vale for the Dialect is 'Core'.</value>
	STATIC PROPERTY Dialect AS XSharpDialect AUTO

	/// <summary>The current compiler setting for the FOVF compiler option.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default vale for this option is 'False'.</value>
	STATIC PROPERTY CompilerOptionFOVF AS LOGIC AUTO

	/// <summary>The System.Reflection.Module for the main application.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    STATIC PROPERTY AppModule AS  System.Reflection.Module AUTO
	#endregion


	/// <summary>The current ANSI setting</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="F:XSharp.Set.Ansi" />
    STATIC PROPERTY Ansi AS LOGIC ;
        GET GetValue<LOGIC>(Set.Ansi);
        SET SetValue<LOGIC>(Set.Ansi, VALUE)

	/// <summary>The current AutoOrder setting (used by the RDD system).</summary>
    /// <seealso cref="P:XSharp.RuntimeState.AutoOpen" />
    /// <seealso cref="P:XSharp.RuntimeState.AutoShareMode" />
    /// <seealso cref="F:XSharp.Set.AutoOrder" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY AutoOrder AS LONG ;
        GET GetValue<LONG>(Set.AutoOrder);
        SET SetValue<LONG>(Set.AutoOrder, VALUE)

	/// <summary>The current AutoOpen setting (used by the RDD system).</summary>
    /// <seealso cref="P:XSharp.RuntimeState.AutoOrder" />
    /// <seealso cref="P:XSharp.RuntimeState.AutoShareMode" />
    /// <seealso cref="F:XSharp.Set.AutoOpen" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY AutoOpen AS LOGIC ;
        GET GetValue<LOGIC>(Set.AutoOpen);
        SET SetValue<LOGIC>(Set.AutoOpen, VALUE)

	/// <summary>The current AutoShareMode setting (used by the RDD system).</summary>
    /// <seealso cref="P:XSharp.RuntimeState.AutoOpen" />
    /// <seealso cref="P:XSharp.RuntimeState.AutoOrder" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY AutoShareMode AS LONG ;
        GET (LONG) GetValue<AutoShareMode>(Set.AutoShare);
        SET SetValue<AutoShareMode>(Set.AutoShare, (AutoShareMode)VALUE)


	/// <summary>The current Century setting (used in DATE &lt;-&gt; STRING conversions).</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
   STATIC PROPERTY Century AS LOGIC ;
        GET GetValue<LOGIC>(Set.Century);
        SET SetValue<LOGIC>(Set.Century, VALUE)

	/// <summary>The current Collation mode (used by the RDD system).</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
   STATIC PROPERTY CollationMode AS CollationMode 
        GET 
			RETURN GetValue<CollationMode>(Set.CollationMode)
		END GET
        SET 
			SetValue<CollationMode>(Set.CollationMode, VALUE)
			IF OnCollationChanged != NULL
				OnCollationChanged(GetInstance(), EVentArgs{})
			ENDIF
		END SET
	END PROPERTY

	/// <summary>The current DateCountry setting mode (used in DATE &lt;-&gt; STRING conversions).</summary>
    /// <seealso cref="P:XSharp.RuntimeState.DateFormat" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
   STATIC PROPERTY DateCountry AS DWORD ;
        GET GetValue<DWORD>(Set.DateCountry);
        SET _SetDateCountry(VALUE)

	/// <summary>The current Date format</summary>
	/// <remarks>This string should contain a combination of DD MM and either YY or YYYY characters.<br/>
	/// For example DD-MM-YYYY for italian date format, MM/DD/YYYY for American date format or DD/MM/YYYY for British Date format.
	/// Note that all other characters except the four groups mentioned above are copied to the output string verbatim.
    /// <note>This value is 'per thread' </note></remarks>
    /// <seealso cref="P:XSharp.RuntimeState.DateCountry" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY DateFormat AS STRING ;
        GET GetValue<STRING>(Set.DateFormat);
        SET _SetDateFormat(VALUE)

    /// <summary>A cached copy of the string that is returned for empty dates, matching the current DateFormat</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
	STATIC PROPERTY NullDateString AS STRING GET GetValue<STRING>(Set.DateFormatEmpty)

	/// <summary>The default number of decimals for new FLOAT values that are created without explicit decimals.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY Decimals AS DWORD ;
        GET GetValue<DWORD>(Set.DECIMALS);
        SET SetValue<DWORD>(Set.DECIMALS, VALUE)


	/// <summary>The default number of decimals for new FLOAT values that are created without explicit decimals.</summary>
    /// <seealso cref="P:XSharp.RuntimeState.ThousandSep" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY DecimalSep AS DWORD ;
        GET GetValue<DWORD>(Set.DecimalSep);
        SET SetValue<DWORD>(Set.DecimalSep, VALUE)

	/// <summary>The default RDD.</summary>
    /// <remarks><note>This value is 'per thread' </note></remarks>
    STATIC PROPERTY DefaultRDD AS STRING ;
        GET GetValue<STRING>(Set.DEFAULTRDD);
        SET SetValue<STRING>(Set.DEFAULTRDD, VALUE)


	/// <summary>RDD Deleted Flag that determines whether to ignore or include records that are marked for deletion.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY Deleted AS LOGIC ;
        GET GetValue<LOGIC>(Set.DELETED);
        SET SetValue<LOGIC>(Set.DELETED, VALUE)

	/// <summary>The default number of digits for new FLOAT values that are created without explicit decimals</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY Digits AS DWORD ;
        GET GetValue<DWORD>(Set.DIGITS);
        SET SetValue<DWORD>(Set.DIGITS, VALUE)

    /// <summary>Logical setting that fixes the number of digits used to display numeric output.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY DigitsFixed AS LOGIC ;
        GET GetValue<LOGIC>(Set.DigitFixed);
        SET SetValue<LOGIC>(Set.DigitFixed, VALUE)


	/// <summary>The DOS Codepage. This gets read at startup from the OS().</summary>
    /// <seealso cref="P:XSharp.RuntimeState.DosEncoding" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY DosCodePage AS LONG 
        GET
            RETURN Convert.ToInt32(GetValue<OBJECT>(Set.DOSCODEPAGE))
		END GET
        SET 
			SetValue<LONG>(Set.DOSCODEPAGE, VALUE) 
			IF OnCodePageChanged != NULL
				OnCodePageChanged(GetInstance(), EventArgs{})
			ENDIF
		END SET
	END PROPERTY

	/// <summary>Date Epoch value that determines how dates without century digits are interpreted.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY Epoch AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.EPOCH));
        SET SetValue<DWORD>(Set.EPOCH, VALUE)

	/// <summary>Date Epoch Year value. This gets set by the SetEpoch() function to the Epoch year % 100.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY EpochYear AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.EPOCHYEAR));

	/// <summary>Date Epoch Century value. This gets set by the SetEpoch() function to the century in which the Epoch year falls.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY EpochCent AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.EPOCHCENT));


	/// <summary>String comparison Exact flag that determines how comparisons with the single '=' characters should be done.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY Exact AS LOGIC ;
        GET GetValue<LOGIC>(Set.EXACT);
        SET SetValue<LOGIC>(Set.EXACT, VALUE)

    /// <summary>Logical setting that fixes the number of decimal digits used to display numbers.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY Fixed AS LOGIC ;
        GET GetValue<LOGIC>(Set.Fixed);
        SET SetValue<LOGIC>(Set.Fixed, VALUE)



	/// <summary>Numeric value that controls the precision of Float comparisons.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
   STATIC PROPERTY FloatDelta AS REAL8 ;
        GET GetValue<REAL8>(Set.FloatDelta);
        SET SetValue<REAL8>(Set.FloatDelta, VALUE)

	/// <summary>Current SetInternational Setting.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
     STATIC PROPERTY International AS CollationMode ;
        GET GetValue<CollationMode>(Set.Intl);
        SET SetValue<CollationMode>(Set.Intl, VALUE)

	/// <summary>Last error that occurred in the RDD subsystem.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY LastRddError AS Exception ;
        GET GetValue<Exception>(Set.LastRddError);
        SET SetValue<Exception>(Set.LastRddError, VALUE)

	/// <summary>Number of tries that were done when the last lock operation failed.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY LockTries AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.LOCKTRIES));
        SET SetValue<DWORD>(Set.LOCKTRIES, VALUE)

    /// <summary>The setting that determines whether to use the High Performance (HP) locking schema for newly created .NTX files</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY HPLocking AS LOGIC ;
        GET GetValue<LOGIC>(Set.HPLOCKING);
        SET SetValue<LOGIC>(Set.HPLOCKING, VALUE)

    /// <summary>The setting that determines whether to use the new locking offset of -1 (0xFFFFFFFF) for .NTX files.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY NewIndexLock AS LOGIC ;
        GET GetValue<LOGIC>(Set.NEWINDEXLOCK);
        SET SetValue<LOGIC>(Set.NEWINDEXLOCK, VALUE)


	/// <summary>The current default MemoBlock size.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY MemoBlockSize AS WORD;
        GET Convert.ToUInt16(GetValue<OBJECT>(Set.MEMOBLOCKSIZE));
        SET SetValue<WORD>(Set.MEMOBLOCKSIZE, VALUE)


	/// <summary>Did the last RDD operation cause a Network Error ?</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY NetErr AS LOGIC;
        GET GetValue<LOGIC>(Set.NETERR);
        SET SetValue<LOGIC>(Set.NETERR, VALUE)

	/// <summary>RDD Optimize Flag  </summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY Optimize AS LOGIC ;
        GET GetValue<LOGIC>(Set.OPTIMIZE);
        SET SetValue<LOGIC>(Set.OPTIMIZE, VALUE)

	/// <summary>The current SetSoftSeek flag.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY SoftSeek AS LOGIC ;
        GET GetValue<LOGIC>(Set.SOFTSEEK);
        SET SetValue<LOGIC>(Set.SOFTSEEK, VALUE)

	/// <summary>The Thousand separator</summary>
    /// <seealso cref="P:XSharp.RuntimeState.DecimalSep" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY ThousandSep AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.THOUSANDSEP));
        SET SetValue<DWORD>(Set.THOUSANDSEP, VALUE)


	/// <summary>Number of tries that were done when the last lock operation failed.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY Unique AS LOGIC ;
        GET GetValue<LOGIC>(Set.UNIQUE);
        SET SetValue<LOGIC>(Set.UNIQUE, VALUE)

	/// <summary>The Windows Codepage. This gets read at startup from the OS().</summary>
    /// <seealso cref="P:XSharp.RuntimeState.WinEncoding" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY WinCodePage AS LONG
	GET
        RETURN Convert.ToInt32(GetValue<OBJECT>(Set.WINCODEPAGE))
	END GET
	SET 
        SetValue<LONG>(Set.WINCODEPAGE, VALUE)
			IF OnCodePageChanged != NULL
				OnCodePageChanged(GetInstance(), EventArgs{})
			ENDIF
	END SET
	END PROPERTY

    /// <summary>The DOS Encoding. This is based on the corrent Win Codepage.</summary>
    /// <seealso cref="P:XSharp.RuntimeState.WinCodePage" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY WinEncoding AS System.Text.Encoding ;
        GET System.Text.Encoding.GetEncoding(WinCodePage)

    /// <summary>The DOS Encoding. This is based on the corrent DOS Codepage.</summary>
    /// <seealso cref="P:XSharp.RuntimeState.DosCodePage" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY DosEncoding AS System.Text.Encoding ;
        GET System.Text.Encoding.GetEncoding(DosCodePage)


	/// <summary>The name of the method that was called in the last late bound method call.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY NoMethod AS STRING ;
        GET GetValue<STRING>(Set.NoMethod);
        SET SetValue<STRING>(Set.NoMethod, VALUE)




	INTERNAL METHOD _SetInternationalClipper() AS VOID
		SELF:_SetThreadValue(Set.AMEXT, "")
		SELF:_SetThreadValue(Set.PMEXT, "")
		SELF:_SetThreadValue(Set.AMPM, FALSE)
		SELF:_SetThreadValue(Set.Century, FALSE)
		SELF:_SetThreadValue(Set.DateCountry, (DWORD)1)
		SELF:_SetThreadValue(Set.Decimals, (DWORD) 2)
		SELF:_SetThreadValue(Set.DECIMALSEP,  (DWORD) 46)		// DOT .
		SELF:_SetThreadValue(Set.THOUSANDSEP, (DWORD) 44)	// COMMA ,
		SELF:_SetThreadValue(Set.DateFormat, "MM/DD/YY")
		SELF:_SetThreadValue(Set.Intl, CollationMode.Clipper)
        SELF:_SetThreadValue(Set.Dict, FALSE)

	INTERNAL METHOD _SetInternationalWindows() AS VOID
		VAR dtInfo	    := System.Globalization.DateTimeFormatInfo.CurrentInfo
		SELF:_SetThreadValue(Set.AMEXT, dtInfo:AMDesignator)
		SELF:_SetThreadValue(Set.PMEXT, dtInfo:PMDesignator)
		VAR separator := dtInfo:TimeSeparator
		IF String.IsNullOrEmpty(separator)
			SELF:_SetThreadValue(Set.TimeSep, (DWORD) 0)
		ELSE
			SELF:_SetThreadValue(Set.TimeSep, (DWORD) separator[0])
		ENDIF
		SELF:_SetThreadValue(Set.AMPM, dtInfo:ShortDatePattern:IndexOf("tt") != -1)
		VAR dateformat  := dtInfo:ShortDatePattern:ToLower()
		// reduce to single m and d
		DO WHILE (dateformat.IndexOf("mm") != -1)
			dateformat		:= dateformat:Replace("mm", "m")
		ENDDO
		// make sure we have a double mm to get double digit dates

		DO WHILE dateformat.IndexOf("dd") != -1
			dateformat		:= dateformat:Replace("dd", "d")
		ENDDO
		// change dates to dd and mm and then everything to upper case
		dateformat := dateformat:Replace("d", "dd"):Replace("m","mm"):ToUpper()
		SELF:_SetThreadValue(Set.Century, dateformat:IndexOf("YYYY",StringComparison.OrdinalIgnoreCase) != -1)
		SELF:_SetThreadValue(Set.DateFormatNet, dateformat:ToUpper():Replace("D","d"):Replace("Y","y"):Replace("/","'/'"))
		SELF:_SetThreadValue(Set.DateFormatEmpty, dateformat:ToUpper():Replace("D"," "):Replace("Y"," "):Replace("M"," "))
		SELF:_SetThreadValue(Set.DateFormat,  dateformat)
		SELF:_SetThreadValue(Set.DateCountry, (DWORD) 1)
		SELF:_SetThreadValue(Set.DECIMALS , (DWORD) 2)
		VAR numberformat := System.Globalization.NumberFormatInfo.CurrentInfo
		SELF:_SetThreadValue(Set.DECIMALSEP, (DWORD) numberformat:NumberDecimalSeparator[0])
		SELF:_SetThreadValue(Set.THOUSANDSEP, (DWORD) numberformat:NumberGroupSeparator[0])
		SELF:_SetThreadValue(Set.EPOCH, (DWORD) 1910)
		SELF:_SetThreadValue(Set.EpochYear, (DWORD) 10)
		SELF:_SetThreadValue(Set.EpochCent, (DWORD) 2000)
		SELF:_SetThreadValue(Set.Intl, CollationMode.Windows)
        SELF:_SetThreadValue(Set.Dict, TRUE)
        // FoxPro settings that do not hurt in other dialects
        SELF:_SetThreadValue(Set.FullPath, TRUE)
        SELF:_SetThreadValue(Set.Space, TRUE)
        SELF:_SetThreadValue(Set.Textmerge, FALSE)
		RETURN


	INTERNAL STATIC METHOD _SetDateFormat(format AS STRING) AS VOID
		format := format:ToUpper()
		// ensure we have dd, mm and yy
		IF format:IndexOf("DD") == -1 .OR. format:IndexOf("MM") == -1 .OR. format:IndexOf("YY") == -1
			RETURN
		ENDIF
		SetValue(Set.DateFormatNet, format:Replace("D","d"):Replace("Y","y"):Replace("/","'/'"))
		SetValue(Set.DateFormatEmpty, format:Replace("D"," "):Replace("Y"," "):Replace("M"," "))
		SetValue(SET.CENTURY, format:Contains("YYYY"))
		SetValue(Set.DATEFORMAT, format)
		SWITCH format
			CASE "MM/DD/YY"
			CASE "MM/DD/YYYY" 
				SetValue(Set.DATECOUNTRY, (DWORD) XSharp.DateCountry.American)	 
			CASE "YY.MM.DD" 
			CASE "YYYY.MM.DD"
				SetValue(Set.DATECOUNTRY, (DWORD) XSharp.DateCountry.Ansi)	  
			CASE "DD/MM/YY"
			CASE "DD/MM/YYYY"
                // What a laugh, the British & french have an identical format. 
				SetValue(Set.DATECOUNTRY, (DWORD)XSharp.DateCountry.British)	
			CASE "DD.MM.YY"
			CASE "DD.MM.YYYY"
				SetValue(Set.DATECOUNTRY, (DWORD)XSharp.DateCountry.German)	
			CASE "DD-MM-YY"
			CASE "DD-MM-YYYY"
				SetValue(Set.DATECOUNTRY, (DWORD)XSharp.DateCountry.Italian)	
			CASE "YY/MM/DD"
			CASE "YYYY/MM/DD"
				SetValue(Set.DATECOUNTRY, (DWORD)XSharp.DateCountry.Japanese)	
			CASE "MM-DD-YY"
			CASE "MM-DD-YYYY"
				SetValue(Set.DATECOUNTRY, (DWORD)XSharp.DateCountry.USA)	
			OTHERWISE
				SetValue(Set.DATECOUNTRY, (DWORD)0)	
		END SWITCH


	INTERNAL STATIC METHOD _SetDateCountry(country AS DWORD) AS VOID
		IF country > 8
			RETURN
		END IF

		SetValue<DWORD>(Set.DateCountry, country)
		
		LOCAL format, year AS STRING
		year := IIF(Century , "YYYY" , "YY")
		SWITCH (DateCountry) country
			CASE XSharp.DateCountry.American
				format := "MM/DD/" + year
			CASE XSharp.DateCountry.Ansi
				format := year + ".MM.DD"
			CASE XSharp.DateCountry.British
			CASE XSharp.DateCountry.French
                // What a laugh, the British & french have an identical format. 
				format := "DD/MM/" + year
			CASE XSharp.DateCountry.German
				format := "DD.MM." + year
			CASE XSharp.DateCountry.Italian
				format := "DD-MM-" + year
			CASE XSharp.DateCountry.Japanese
				format := year + "/MM/DD"
			CASE XSharp.DateCountry.USA
				format := "MM-DD-" + year
			OTHERWISE
				format := "MM/DD/" + year
		END SWITCH
		// this will adjust DateFormatNet, DateFormatEmpty etc, but also DateCountry again
		_SetDateFormat(format) 



	PRIVATE _workareas AS WorkAreas
	/// <summary>The workarea information for the current Thread.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
	PUBLIC STATIC PROPERTY Workareas AS WorkAreas
	GET
       LOCAL inst AS RuntimeState
        inst := GetInstance()
		IF inst:_workareas == NULL_OBJECT
			inst:_workareas := WorkAreas{}
		ENDIF
		RETURN inst:_workareas
	END GET
    END PROPERTY
    /// <exclude />
    STATIC METHOD PushCurrentWorkarea(dwArea AS DWORD) AS VOID
        RuntimeState.WorkAreas:PushCurrentWorkArea(dwArea)
    /// <exclude />    
    STATIC METHOD PopCurrentWorkarea() AS DWORD
        RETURN RuntimeState.WorkAreas:PopCurrentWorkArea()

	PRIVATE _collationTable AS BYTE[]
    /// <summary>Current collation table.</summary>
    
	PUBLIC STATIC PROPERTY CollationTable AS BYTE[]
	GET
		LOCAL coll AS BYTE[]
		coll := GetInstance():_collationTable 
		IF coll == NULL .OR. coll :Length < 256
			_SetCollation("Generic")
			coll := GetInstance():_collationTable := GetValue<BYTE[]>(SET.CollationTable)
		ENDIF
		RETURN coll
	END GET
	SET
		GetInstance():_collationTable  := VALUE
		SetValue(Set.CollationTable, VALUE)
		IF OnCollationChanged != NULL
			OnCollationChanged(GetInstance(), EventArgs{})
		ENDIF
	END SET
	END PROPERTY

	STATIC INTERNAL _macrocompilerType   AS System.Type
    STATIC INTERNAL _macrocompiler       AS IMacroCompiler
    STATIC INTERNAL _macroresolver       AS MacroCompilerResolveAmbiguousMatch
    /// <summary>Active Macro compiler</summary>
    /// <remarks><note>This value is NOT 'per thread' but global for all threads.</note></remarks>
    PUBLIC STATIC PROPERTY MacroCompiler AS IMacroCompiler
        GET
            IF _macrocompiler == NULL 
                _LoadMacroCompiler()
            ENDIF
            RETURN _macrocompiler
        END GET
        SET
            _macrocompiler := VALUE
        END SET
    END PROPERTY
        
    /// <summary>Active Macro compiler</summary>
    /// <remarks><note>This value is NOT 'per thread' but global for all threads.</note></remarks>
    PUBLIC STATIC PROPERTY MacroResolver AS MacroCompilerResolveAmbiguousMatch
        GET
            if _macroResolver == null
                IF _macroCompiler IS IMacroCompiler2 VAR mc
                    _macroResolver := mc:Resolver 
                ENDIF
            endif
            RETURN _macroresolver
        END GET
        SET
            _macroresolver := VALUE
            IF _macroCompiler IS IMacroCompiler2 VAR mc
                mc:Resolver := Value
            ENDIF
        END SET
    END PROPERTY    
	/// <summary>This event is thrown when the codepage of the runtimestate is changed</summary>
    /// <remarks>Clients can refresh cached information by registering to this event</remarks>
	PUBLIC STATIC EVENT OnCodePageChanged AS EventHandler
	/// <summary>This event is thrown when the collation of the runtimestate is changed</summary>
    /// <remarks>Clients can refresh cached information by registering to this event</remarks>
	PUBLIC STATIC EVENT OnCollationChanged AS EventHandler

    PRIVATE STATIC METHOD _LoadMacroCompiler() AS VOID
        IF _macroCompilerType == NULL_OBJECT
            VAR oMacroAsm := AssemblyHelper.Load("XSharp.MacroCompiler")
		    IF oMacroAsm != NULL_OBJECT
			    LOCAL oType AS System.Type
			    oType := oMacroAsm:GetType("XSharp.Runtime.MacroCompiler",FALSE,TRUE)
			    IF oType != NULL_OBJECT
				    // create instance of this type
				    IF TYPEOF(IMacroCompiler):IsAssignableFrom(oType)
					    _macroCompilerType := oType
				    ELSE
					    THROW Error{EG_CORRUPTION, "", "Could not create the macro compiler from the type "+ otype:Fullname+" in the assembly "+oMacroAsm:Location}
				    ENDIF
			    ELSE
				    THROW Error{EG_CORRUPTION, "", "Could not load the macro compiler class in the assembly "+oMacroAsm:Location}
                ENDIF
            ELSE
                // AssemblyHelper.Load will throw an exception
                NOP 
		    ENDIF
        ENDIF
        IF _macroCompilerType != NULL_OBJECT
            _macroCompiler := Activator:CreateInstance(_macroCompilerType) ASTYPE IMacroCompiler
            IF _macroCompiler IS IMacroCompiler2 VAR mc
                mc:Resolver := _macroResolver
            ENDIF
		ENDIF
		RETURN


    /// <overloads>
    /// <summary>Compare 2 strings respecting the runtime string comparison rules.</summary>
    /// </overloads>
    /// <include file="CoreComments.xml" path="Comments/StringCompare/*" />
    /// <param name="strLHS">The first string .</param>
    /// <param name="strRHS">The second string.</param>
    /// <remarks>
    /// The StringCompare method takes into account: <br/>
    /// - the setting of SetExact()<br/>
    /// - the setting of VO13 of the main app<br/>
    /// - the setting of Collation mode <br/>
    /// This method respects the current setting of SetCollation(): <br/>
    /// - When the current collationmode is Clipper or Windows then a Unicode - Ansi conversions will be performed.
    /// The Clipper collation uses the current DOS codepage and the Windows collation the current Windows codepage.<br/>
    /// - When the current collationmode is Unicode or Ordinal then the original strings will be compared.
    /// </remarks>
    STATIC METHOD StringCompare(strLHS AS STRING, strRHS AS STRING) AS INT
       LOCAL ret AS INT
        // Only when vo13 is off and SetExact = TRUE
        IF !RuntimeState.CompilerOptionVO13 .AND. RuntimeState.Exact
            RETURN String.Compare( strLHS,  strRHS)
        ENDIF                            
        IF Object.ReferenceEquals(strLHS, strRHS)
            ret := 0
        ELSEIF strLHS == NULL
            IF strRHS == NULL			// null and null are equal
                ret := 0
            ELSE
                ret := -1				// null precedes a string
            ENDIF
        ELSEIF strRHS == NULL			// a string comes after null
            ret := 1
        ELSE							// both not null
            // With Not Exact comparison we only compare the length of the RHS string
            // and we always use the unicode comparison because that is what vulcan does
            // This is done to make sure that >= and <= will also return TRUE when the LHS is longer than the RHS
            // The ordinal comparison can be done here because when both strings start with the same characters
            // then equality is guaranteed regardless of collations or other rules.
            // collations and other rules are really only relevant when both strings are different
            IF  !RuntimeState.Exact
                LOCAL lengthRHS AS INT
                lengthRHS := strRHS:Length
                IF lengthRHS == 0 .OR. lengthRHS <= strLHS:Length  .AND. String.Compare( strLHS, 0, strRHS, 0, lengthRHS , StringComparison.Ordinal ) == 0
                    RETURN 0
                ENDIF
            ENDIF
            ret := StringCompareCollation(strLHS, strRHS)
        ENDIF
        RETURN ret

    /// <include file="CoreComments.xml" path="Comments/StringCompare/*" />
    /// <param name="strLHS">The first string .</param>
    /// <param name="strRHS">The second string.</param>
    /// <remarks>
    /// This method only checks for SetCollation and does not check fot SetExact() or the VO13 setting
    /// </remarks>
    STATIC METHOD StringCompareCollation(strLHS AS STRING, strRHS AS STRING) AS INT
       LOCAL ret AS INT
        // either exact or RHS longer than LHS
        VAR mode := RuntimeState.CollationMode 
        SWITCH mode
        CASE CollationMode.Windows
            IF System.Environment.OSVersion:Platform == System.PlatformID.Win32NT
                ret := XSharp.StringHelpers.CompareWindows(strLHS, strRHS)
            ELSE
                ret := String.Compare(strLHS, strRHS)
            ENDIF
        CASE CollationMode.Clipper
            ret := XSharp.StringHelpers.CompareClipper(strLHS, strRHS) 
        CASE CollationMode.Unicode
            ret := String.Compare(strLHS, strRHS)
        OTHERWISE
            ret := String.CompareOrdinal(strLHS, strRHS)
        END SWITCH
        RETURN ret

    /// <inheritdoc cref="M:XSharp.RuntimeState.StringCompare(System.String,System.String)"/>
    /// <summary>Compare 2 byte arrays respecting the runtime string comparison rules.</summary>
    /// <param name="aLHS">The first list of bytes.</param>
    /// <param name="aRHS">The second list of bytes.</param>
    /// <param name="nLen">The # of bytes to compare.</param>
    /// <remarks>This method works on BYTE arrays and is used by the RDD system. <br/>
    /// This method respects the current setting of SetCollation(): <br/>
    /// - When the current collationmode is Clipper or Windows then no Ansi - Unicode conversions will be done.<br/>
    /// - When the current collationmode is Unicode or Ordinal then the byte arrays will be converted to Unicode before
    /// the comparison is executed. 
    /// </remarks>
    STATIC METHOD StringCompare(aLHS AS BYTE[], aRHS AS BYTE[], nLen AS INT) AS INT
        SWITCH CollationMode
        CASE CollationMode.Clipper
            RETURN XSharp.StringHelpers.CompareClipper(aLHS, aRHS, nLen)
        CASE CollationMode.Windows
            IF System.Environment.OSVersion:Platform == System.PlatformID.Win32NT
                RETURN XSharp.StringHelpers.CompareWindows(aLHS, aRHS, nLen)
            ELSE
                VAR strLHS := RuntimeState.WinEncoding:GetString(aLHS, 0, nLen)
                VAR strRHS := RuntimeState.WinEncoding:GetString(aRHS, 0, nLen)
                RETURN String.Compare(strLHS, strRHS)
            ENDIF
        CASE CollationMode.Unicode
            VAR strLHS := RuntimeState.WinEncoding:GetString(aLHS, 0, nLen)
            VAR strRHS := RuntimeState.WinEncoding:GetString(aRHS, 0, nLen)
            RETURN String.Compare(strLHS, strRHS)
        OTHERWISE
            VAR strLHS := RuntimeState.WinEncoding:GetString(aLHS, 0, nLen)
            VAR strRHS := RuntimeState.WinEncoding:GetString(aRHS, 0, nLen)
            RETURN String.CompareOrdinal(strLHS, strRHS)
        END SWITCH
        RETURN 0
END CLASS




