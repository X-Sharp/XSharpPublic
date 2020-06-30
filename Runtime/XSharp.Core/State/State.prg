//
// Copyright (c) XSharp B.V.  All Rights Reserved.  
// Licensed under the Apache License, Version 2.0.  
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Threading
USING System.Diagnostics
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
	PRIVATE INITONLY _thread AS Thread
	PRIVATE STATIC _shutdown := FALSE AS LOGIC  // To prevent creating state when shutting down
	// Static Methods and Constructor
	PRIVATE STATIC currentState := ThreadLocal<RuntimeState>{ {=>  initialState:Clone()},TRUE }  AS ThreadLocal<RuntimeState> 
	STATIC CONSTRUCTOR
		initialState	:= RuntimeState{TRUE}

	/// <summary>Retrieve the runtime state for the current thread</summary>
	PUBLIC STATIC METHOD GetInstance() AS RuntimeState
		RETURN currentState:Value

    [DebuggerBrowsable(DebuggerBrowsableState.Never)];
	PRIVATE oSettings AS Dictionary<XSharp.Set, OBJECT>
    /// <summary>The dictionary that stores most of the settings in the runtime state. The key to the index is the number from the Set Enum</summary>
    /// <seealso cref="T:XSharp.Set" >Set Enum</seealso>
    PUBLIC PROPERTY Settings AS Dictionary<XSharp.Set, OBJECT> GET oSettings

	PRIVATE CONSTRUCTOR(initialize AS LOGIC)       
		VAR oThread := Thread.CurrentThread
        SELF:_thread := oThread
		oSettings := Dictionary<XSharp.Set, OBJECT>{}
		IF initialize
			SELF:BreakLevel := 0
            // Set default values
            FOREACH nSet AS XSharp.Set IN System.Enum.GetValues(typeof(XSharp.Set))
                VAR def := RuntimeStateDefaultValue(nSet)
                IF def != NULL
                    SELF:_SetThreadValue(nSet, def)
                ENDIF
            NEXT
            // The following were skipped above bcause of the NULL check
            SELF:_SetThreadValue<Exception>(Set.FileException,NULL)
            SELF:_SetThreadValue<Exception>(Set.LastRddError,NULL)
            SELF:_SetThreadValue<Exception>(Set.Patharray,NULL)
            SELF:_SetThreadValue<BYTE[]>(Set.CollationTable, NULL )
			IF IsRunningOnWindows()
                SELF:_SetThreadValue<LONG>(Set.DosCodepage, Win32.GetDosCodePage())
                SELF:_SetThreadValue<LONG>(Set.WinCodepage, Win32.GetWinCodePage())
            ELSE
                SELF:_SetThreadValue(Set.CollationMode, CollationMode.Unicode )
            ENDIF
			// Date and time settings
			SELF:_SetInternationalWindows()


		ENDIF
		RETURN
    /// <exclude />
//	DESTRUCTOR()
//		// What do we need to clean ?
//        IF oSettings != NULL
//			oSettings:Clear()
//		ENDIF


    /// <summary>This method closes all open Workareas for all threads. It is automatically called ad shutdown.</summary>
    /// <remarks>It is usually better to manage the lifetime of your Workarea yourself in code, so don't trust on the runtime to close the Workareas.</remarks>
    STATIC METHOD CloseWorkareasForAllThreads() AS VOID
        TRY
            _shutdown := TRUE   // prevent creating state when shutting down
            FOREACH VAR state IN currentState:Values
                IF state:_Workareas != NULL
                    state:_Workareas:CloseAll()
                ENDIF
            NEXT
        CATCH e AS Exception
            System.Diagnostics.Debug.WriteLine(e:ToString())
        END TRY
	PRIVATE METHOD Clone() AS RuntimeState
		LOCAL oNew AS RuntimeState
        IF Thread.CurrentThread == initialState:_thread .OR. _shutdown
            RETURN initialState
        ENDIF
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
	PUBLIC PROPERTY Name AS STRING
    GET
        IF SELF:_thread == NULL
            RETURN "ThreadState for Unknown Thread"
        ELSE
            RETURN "ThreadState for Thread "+SELF:_thread:ManagedThreadId:ToString()
        ENDIF
    END GET
    END PROPERTY
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
	PUBLIC STATIC METHOD GetValue<T> (nSetting AS XSharp.Set) AS T
        IF _shutdown
            // There is no RuntimeState when shutting down            
            RETURN DEFAULT(T)
        ENDIF
		RETURN currentState:Value:_GetThreadValue<T>(nSetting);

	/// <summary>Set a value for the state of the current Thread.</summary>
	/// <param name="nSetting">Setting number to retrieve. Must be defined in the SET enum.</param>
	/// <param name="oValue">The new value for the setting.</param>
	/// <typeparam name="T">The return type expected for this setting.</typeparam>
	/// <returns>The previous value, or a default value of type T when the setting was not yetr defined.</returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
	PUBLIC STATIC METHOD SetValue<T> (nSetting AS XSharp.Set, oValue AS T) AS T
        IF _shutdown
            // Suppress updating RuntimeState when shutting down
            RETURN oValue
        ENDIF
		RETURN currentState:Value:_SetThreadValue<T>(nSetting, oValue)
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
	PRIVATE METHOD _GetThreadValue<T> (nSetting AS XSharp.Set) AS T
		BEGIN LOCK oSettings
            IF oSettings:TryGetValue(nSetting, OUT VAR result)
				RETURN (T) result
			ENDIF
		END LOCK
		RETURN DEFAULT(T) 
    [MethodImpl(MethodImplOptions.AggressiveInlining)];
	PRIVATE METHOD _SetThreadValue<T>(nSetting AS XSharp.Set, oValue AS T) AS T
		LOCAL result AS T
		BEGIN LOCK oSettings
			IF oSettings:TryGetValue(nSetting, OUT VAR oResult)
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
	/// <summary>Gets / Sets the current Workarea number.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY CurrentWorkarea AS DWORD ;
        GET Workareas:CurrentWorkareaNO ;
        SET Workareas:CurrentWorkareaNO  := value

    /// <summary>The current compiler setting for the OVF compiler option.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default vale for this option is 'False'.</value>
	STATIC PROPERTY CompilerOptionOVF AS LOGIC AUTO

    /// <summary>The current compiler setting for the X# Dialect.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default vale for the Dialect is 'Core'.</value>
	STATIC PROPERTY Dialect AS XSharpDialect  AUTO

	/// <summary>The current compiler setting for the FOVF compiler option.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default vale for this option is 'False'.</value>
	STATIC PROPERTY CompilerOptionFOVF AS LOGIC AUTO

	/// <summary>The System.Reflection.Module for the main application.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    STATIC PROPERTY AppModule AS  System.Reflection.Module AUTO
	#endregion

	/// <summary>The last file found with File(). This is the name that FPathName() returns.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.File(System.String)" />
    /// <seealso cref="M:XSharp.Core.Functions.FPathName" />
    /// <seealso cref="F:XSharp.Set.LastFound" />
    STATIC PROPERTY LastFound AS STRING ;
        GET GetValue<STRING>(Set.LastFound);
        SET SetValue<STRING>(Set.LastFound, value)

	/// <summary>The last File IO error number</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.FError" />
    /// <seealso cref="M:XSharp.Core.Functions.FError(System.UInt32)" />
    /// <seealso cref="F:XSharp.Set.FileError" />
    STATIC PROPERTY FileError AS DWORD ;
        GET GetValue<DWORD>(Set.FileError);
        SET SetValue<DWORD>(Set.FileError, value)

	/// <summary>The last file IO Exception</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.FException" />
    /// <seealso cref="F:XSharp.Set.FileException" />
    STATIC PROPERTY FileException AS Exception ;
        GET GetValue<Exception>(Set.FileException);
        SET SetValue<Exception>(Set.FileException, value)

	/// <summary>The current ANSI setting</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetAnsi" />
    /// <seealso cref="M:XSharp.Core.Functions.SetAnsi(System.Boolean)" />
    /// <seealso cref="F:XSharp.Set.Ansi" />
    STATIC PROPERTY Ansi AS LOGIC ;
        GET GetValue<LOGIC>(Set.Ansi);
        SET SetValue<LOGIC>(Set.Ansi, value)

	/// <summary>The current AutoOrder setting (used by the RDD system).</summary>
    /// <seealso cref="P:XSharp.RuntimeState.AutoOpen" />
    /// <seealso cref="P:XSharp.RuntimeState.AutoShareMode" />
    /// <seealso cref="F:XSharp.Set.AutoOrder" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY AutoOrder AS LONG ;
        GET GetValue<LONG>(Set.AutoOrder);
        SET SetValue<LONG>(Set.AutoOrder, value)

	/// <summary>The current AutoOpen setting (used by the RDD system).</summary>
    /// <seealso cref="P:XSharp.RuntimeState.AutoOrder" />
    /// <seealso cref="P:XSharp.RuntimeState.AutoShareMode" />
    /// <seealso cref="F:XSharp.Set.AutoOpen" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY AutoOpen AS LOGIC ;
        GET GetValue<LOGIC>(Set.AutoOpen);
        SET SetValue<LOGIC>(Set.AutoOpen, value)

	/// <summary>The current AutoShareMode setting (used by the RDD system).</summary>
    /// <seealso cref="P:XSharp.RuntimeState.AutoOpen" />
    /// <seealso cref="P:XSharp.RuntimeState.AutoOrder" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY AutoShareMode AS LONG ;
        GET (LONG) GetValue<AutoShareMode>(Set.Autoshare);
        SET SetValue<AutoShareMode>(Set.Autoshare, (AutoShareMode)value)


	/// <summary>The current Century setting (used in DATE &lt;-&gt; STRING conversions).</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetCentury" />
    /// <seealso cref="M:XSharp.Core.Functions.SetCentury(System.Boolean)" />
    /// <seealso cref="F:XSharp.Set.Century" />
   STATIC PROPERTY Century AS LOGIC ;
        GET GetValue<LOGIC>(Set.Century);
        SET SetValue<LOGIC>(Set.Century, value)

	/// <summary>The current Collation mode (used by the RDD system).</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="F:XSharp.Set.CollationMode" />
   STATIC PROPERTY CollationMode AS CollationMode 
        GET 
			RETURN GetValue<CollationMode>(Set.CollationMode)
		END GET
        SET 
			SetValue<CollationMode>(Set.CollationMode, value)
			IF OnCollationChanged != NULL
				OnCollationChanged(GetInstance(), EventArgs{})
			ENDIF
		END SET
	END PROPERTY

	/// <summary>The current DateCountry setting mode (used in DATE &lt;-&gt; STRING conversions).</summary>
    /// <seealso cref="P:XSharp.RuntimeState.DateFormat" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDateCountry" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDateCountry(System.UInt32)" />
    /// <seealso cref="F:XSharp.Set.DateCountry" />
   STATIC PROPERTY DateCountry AS DWORD ;
        GET (DWORD) GetValue<XSharp.DateCountry>(Set.DateCountry);
        SET RuntimeState:GetInstance():_SetDateCountry( (XSharp.DateCountry) value)

	/// <summary>The current Date format</summary>
	/// <remarks>This string should contain a combination of DD MM and either YY or YYYY characters.<br/>
	/// For example DD-MM-YYYY for italian date format, MM/DD/YYYY for American date format or DD/MM/YYYY for British Date format.
	/// Note that all other characters except the four groups mentioned above are copied to the output string verbatim.
    /// <note>This value is 'per thread' </note></remarks>
    /// <seealso cref="P:XSharp.RuntimeState.DateCountry" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.GetDateFormat" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDateFormat(System.String)" />
    /// <seealso cref="F:XSharp.Set.DateFormat" />
    STATIC PROPERTY DateFormat AS STRING ;
        GET GetValue<STRING>(Set.DateFormat);
        SET RuntimeState.GetInstance():_SetDateFormat(value)

    /// <summary>A cached copy of the string that is returned for empty dates, matching the current DateFormat</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="F:XSharp.Set.DateFormatEmpty" />
	STATIC PROPERTY NullDateString AS STRING GET GetValue<STRING>(Set.DateFormatEmpty)

	/// <summary>The default number of decimals for new FLOAT values that are created without explicit decimals.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDecimal" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDecimal(System.UInt32)" />
    /// <seealso cref="F:XSharp.Set.Decimals" />
    STATIC PROPERTY Decimals AS DWORD ;
        GET GetValue<DWORD>(Set.Decimals);
        SET SetValue<DWORD>(Set.Decimals, value)


	/// <summary>The default number of decimals for new FLOAT values that are created without explicit decimals.</summary>
    /// <seealso cref="P:XSharp.RuntimeState.ThousandSep" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDecimalSep" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDecimalSep(System.UInt32)" />
    /// <seealso cref="F:XSharp.Set.DecimalSep" />
    STATIC PROPERTY DecimalSep AS DWORD ;
        GET GetValue<DWORD>(Set.DecimalSep);
        SET SetValue<DWORD>(Set.DecimalSep, value)

	/// <summary>The default RDD.</summary>
    /// <remarks><note>This value is 'per thread' </note></remarks>
    /// <seealso cref="M:XSharp.Core.Functions.SetDefault" />
    STATIC PROPERTY DefaultRDD AS STRING ;
        GET GetValue<STRING>(Set.DefaultRdd);
        SET SetValue<STRING>(Set.DefaultRdd, value)


	/// <summary>RDD Deleted Flag that determines whether to ignore or include records that are marked for deletion.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDeleted" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDeleted(System.Boolean)" />
    /// <seealso cref="F:XSharp.Set.Deleted" />
    STATIC PROPERTY Deleted AS LOGIC ;
        GET GetValue<LOGIC>(Set.Deleted);
        SET SetValue<LOGIC>(Set.Deleted, value)

	/// <summary>Name of the RDD uses for DELIM operations.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY DelimRDD AS STRING ;
        GET GetValue<STRING>(Set.DelimRDD);
        SET SetValue<STRING>(Set.DelimRDD, value)

	/// <summary>Field delimiters for DELIM operations.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY FieldDelimiter AS STRING ;
        GET GetValue<STRING>(Set.FieldDelimiter);
        SET SetValue<STRING>(Set.FieldDelimiter, value)

	/// <summary>String delimiters for DELIM operations.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY StringDelimiter AS STRING ;
        GET GetValue<STRING>(Set.Delimiters);
        SET SetValue<STRING>(Set.Delimiters, value)

	/// <summary>The default number of digits for new FLOAT values that are created without explicit decimals</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDigit" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDigit(System.UInt32)" />
    /// <seealso cref="F:XSharp.Set.Digits" />
    STATIC PROPERTY Digits AS DWORD ;
        GET GetValue<DWORD>(Set.Digits);
        SET SetValue<DWORD>(Set.Digits, value)

    /// <summary>Logical setting that fixes the number of digits used to display numeric output.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDigitFixed" />
    /// <seealso cref="M:XSharp.Core.Functions.SetDigitFixed(System.Boolean)" />
    /// <seealso cref="F:XSharp.Set.DigitFixed" />
    STATIC PROPERTY DigitsFixed AS LOGIC ;
        GET GetValue<LOGIC>(Set.DigitFixed);
        SET SetValue<LOGIC>(Set.DigitFixed, value)


	/// <summary>The DOS Codepage. This gets read at startup from the OS().</summary>
    /// <seealso cref="P:XSharp.RuntimeState.DosEncoding" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY DosCodePage AS LONG 
        GET
            RETURN GetValue<LONG>(Set.DosCodepage)
		END GET
        SET 
			SetValue<LONG>(Set.DosCodepage, value) 
			IF OnCodePageChanged != NULL
				OnCodePageChanged(GetInstance(), EventArgs{})
			ENDIF
		END SET
	END PROPERTY

	/// <summary>Should text files, such as memo files be written with a closing EOF = chr(26) character.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="F:XSharp.Set.Eof" />
    STATIC PROPERTY Eof AS LOGIC ;
        GET GetValue<LOGIC>(Set.Eof);
        SET SetValue<LOGIC>(Set.Eof, value)


	/// <summary>End of line character to be used by the runtime. Defaults to CR + LF (13 + 13).</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="F:XSharp.Set.Eol" />
    STATIC PROPERTY Eol AS STRING ;
        GET GetValue<STRING>(Set.Eol);
        SET SetValue<STRING>(Set.Eol, value)


	/// <summary>Date Epoch value that determines how dates without century digits are interpreted.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetEpoch" />
    /// <seealso cref="M:XSharp.Core.Functions.SetEpoch(System.UInt32)" />
    /// <seealso cref="F:XSharp.Set.Epoch" />

    STATIC PROPERTY Epoch AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.Epoch));
        SET SetValue<DWORD>(Set.Epoch, value)

	/// <summary>Date Epoch Year value. This gets set by the SetEpoch() function to the Epoch year % 100.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetEpoch" />
    /// <seealso cref="M:XSharp.Core.Functions.SetEpoch(System.UInt32)" />
    /// <seealso cref="F:XSharp.Set.EpochYear" />
    STATIC PROPERTY EpochYear AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.EpochYear));

	/// <summary>Date Epoch Century value. This gets set by the SetEpoch() function to the century in which the Epoch year falls.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetEpoch" />
    /// <seealso cref="M:XSharp.Core.Functions.SetEpoch(System.UInt32)" />
    /// <seealso cref="F:XSharp.Set.EpochCent" />
    STATIC PROPERTY EpochCent AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.EpochCent));


	/// <summary>String comparison Exact flag that determines how comparisons with the single '=' characters should be done.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetExact" />
    /// <seealso cref="M:XSharp.Core.Functions.SetExact(System.Boolean)" />
    /// <seealso cref="F:XSharp.Set.Exact" />
    STATIC PROPERTY Exact AS LOGIC ;
        GET GetValue<LOGIC>(Set.Exact);
        SET SetValue<LOGIC>(Set.Exact, value)

    /// <summary>Logical setting that fixes the number of decimal digits used to display numbers.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetFixed" />
    /// <seealso cref="M:XSharp.Core.Functions.SetFixed(System.Boolean)" />
    /// <seealso cref="F:XSharp.Set.Fixed" />
    STATIC PROPERTY Fixed AS LOGIC ;
        GET GetValue<LOGIC>(Set.Fixed);
        SET SetValue<LOGIC>(Set.Fixed, value)



	/// <summary>Numeric value that controls the precision of Float comparisons.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.RT.Functions.SetFloatDelta" />
    /// <seealso cref="M:XSharp.RT.Functions.SetFloatDelta(System.Double)" />
    /// <seealso cref="F:XSharp.Set.Floatdelta" />
   STATIC PROPERTY FloatDelta AS REAL8 ;
        GET GetValue<REAL8>(Set.Floatdelta);
        SET SetValue<REAL8>(Set.Floatdelta, value)

	/// <summary>Current SetInternational Setting.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetInternational" />
    /// <seealso cref="M:XSharp.Core.Functions.SetInternational(System.String)" />
    /// <seealso cref="F:XSharp.Set.Intl" />
     STATIC PROPERTY International AS CollationMode ;
        GET GetValue<CollationMode>(Set.Intl);
        SET SetValue<CollationMode>(Set.Intl, value)

	/// <summary>Last error that occurred in the RDD subsystem.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="F:XSharp.Set.LastRddError" />
    STATIC PROPERTY LastRddError AS Exception ;
        GET GetValue<Exception>(Set.LastRddError);
        SET SetValue<Exception>(Set.LastRddError, value)

	/// <summary>Number of tries that were done when the last lock operation failed.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetEpoch" />
    /// <seealso cref="M:XSharp.Core.Functions.SetEpoch(System.UInt32)" />
    /// <seealso cref="F:XSharp.Set.EpochCent" />
    STATIC PROPERTY LockTries AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.Locktries));
        SET SetValue<DWORD>(Set.Locktries, value)

    /// <summary>The setting that determines whether to use the High Performance (HP) locking schema for newly created .NTX files</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="F:XSharp.Set.HpLocking" />
    STATIC PROPERTY HPLocking AS LOGIC ;
        GET GetValue<LOGIC>(Set.HpLocking);
        SET SetValue<LOGIC>(Set.HpLocking, value)

    /// <summary>The setting that determines whether to use the new locking offset of -1 (0xFFFFFFFF) for .NTX files.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="F:XSharp.Set.NewIndexLock" />
    STATIC PROPERTY NewIndexLock AS LOGIC ;
        GET GetValue<LOGIC>(Set.NewIndexLock);
        SET SetValue<LOGIC>(Set.NewIndexLock, value)


	/// <summary>The current default MemoBlock size.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="F:XSharp.Set.MemoBlockSize" />
    STATIC PROPERTY MemoBlockSize AS WORD;
        GET Convert.ToUInt16(GetValue<OBJECT>(Set.MemoBlockSize));
        SET SetValue<WORD>(Set.MemoBlockSize, value)


	/// <summary>Did the last RDD operation cause a Network Error ?</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="F:XSharp.Set.Neterr" />
    STATIC PROPERTY NetErr AS LOGIC;
        GET GetValue<LOGIC>(Set.Neterr);
        SET SetValue<LOGIC>(Set.Neterr, value)

	/// <summary>RDD Optimize Flag  </summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="F:XSharp.Set.Optimize" />
    STATIC PROPERTY Optimize AS LOGIC ;
        GET GetValue<LOGIC>(Set.Optimize);
        SET SetValue<LOGIC>(Set.Optimize, value)

	/// <summary>The current SetSoftSeek flag.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetSoftSeek" />
    /// <seealso cref="M:XSharp.Core.Functions.SetSoftSeek(System.Boolean)" />
    /// <seealso cref="F:XSharp.Set.Softseek" />
    STATIC PROPERTY SoftSeek AS LOGIC ;
        GET GetValue<LOGIC>(Set.Softseek);
        SET SetValue<LOGIC>(Set.Softseek, value)

	/// <summary>The Thousand separator</summary>
    /// <seealso cref="P:XSharp.RuntimeState.DecimalSep" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetThousandSep" />
    /// <seealso cref="M:XSharp.Core.Functions.SetThousandSep(System.UInt32)" />
    /// <seealso cref="F:XSharp.Set.ThousandSep" />
    STATIC PROPERTY ThousandSep AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.ThousandSep));
        SET SetValue<DWORD>(Set.ThousandSep, value)


	/// <summary>Should indexes by default be created with the 'Unique setting'.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="M:XSharp.Core.Functions.SetUnique" />
    /// <seealso cref="M:XSharp.Core.Functions.SetUnique(System.Boolean)" />
    /// <seealso cref="F:XSharp.Set.Unique" />
    STATIC PROPERTY Unique AS LOGIC ;
        GET GetValue<LOGIC>(Set.Unique);
        SET SetValue<LOGIC>(Set.Unique, value)

	/// <summary>The Windows Codepage. This gets read at startup from the OS().</summary>
    /// <seealso cref="P:XSharp.RuntimeState.WinEncoding" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="F:XSharp.Set.WinCodepage" />
    STATIC PROPERTY WinCodePage AS LONG
	GET
		RETURN GetValue<LONG>(Set.WinCodepage)
	END GET
	SET 
        SetValue<LONG>(Set.WinCodepage, value)
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
    /// <seealso cref="F:XSharp.Set.NoMethod" />
    STATIC PROPERTY NoMethod AS STRING ;
        GET GetValue<STRING>(Set.NoMethod);
        SET SetValue<STRING>(Set.NoMethod, value)


    INTERNAL METHOD _SetInternationalClipper() AS VOID
        SELF:_SetThreadValue<DWORD>(Set.DecimalSep, 46)		// DOT .
        SELF:_SetThreadValue<DWORD>(Set.ThousandSep,44)	// COMMA ,
        SELF:_SetThreadValue(Set.Intl, CollationMode.Clipper)
        SELF:_SetThreadValue(Set.Dict, FALSE)
        SELF:_SetThreadValue(Set.Century, FALSE)
        SELF:_SetDateCountry(DateCountry.American)
        SELF:_SetTimeFormat("hh:MM:SS")

    INTERNAL METHOD _SetInternationalWindows() AS VOID
        VAR numberformat := System.Globalization.NumberFormatInfo.CurrentInfo
        SELF:_SetThreadValue<DWORD>(Set.DecimalSep, numberformat:NumberDecimalSeparator[0])
        SELF:_SetThreadValue<DWORD>(Set.ThousandSep, numberformat:NumberGroupSeparator[0])
        SELF:_SetThreadValue<DWORD>(Set.Epoch, 1910)
        SELF:_SetThreadValue<DWORD>(Set.EpochYear, 10)
        SELF:_SetThreadValue<DWORD>(Set.EpochCent, 2000)
        SELF:_SetThreadValue(Set.Intl, CollationMode.Windows)
        SELF:_SetThreadValue(Set.Dict, TRUE)
        // FoxPro settings that do not hurt in other dialects
        SELF:_SetDateFormatSystem()
        SELF:_SetTimeFormatSystem()
		RETURN


    INTERNAL METHOD _SetTimeFormat(format AS STRING) AS VOID
        IF String.IsNullOrEmpty(format)
            SELF:_SetTimeFormatSystem()
        ELSE
            format := format:ToUpper()
            IF format:EndsWith("TT")
		        SELF:_SetThreadValue(Set.AmExt, "AM")
		        SELF:_SetThreadValue(Set.PmExt, "PM")
		        SELF:_SetThreadValue(Set.AmPm, TRUE)
            ELSE
		        SELF:_SetThreadValue(Set.AmExt, "")
		        SELF:_SetThreadValue(Set.PmExt, "")
		        SELF:_SetThreadValue(Set.AmPm, FALSE)
            ENDIF
            SELF:_SetThreadValue(Set.Timesep, Asc(SubStr3(format, 3,1)))
            SELF:_SetThreadValue(Set.Timeformat, format)
       ENDIF

	INTERNAL METHOD _SetDateFormat(format AS STRING) AS VOID
		format := format:ToUpper()
		// ensure we have dd, mm and yy
		IF format:IndexOf("DD") == -1 .OR. format:IndexOf("MM") == -1 .OR. format:IndexOf("YY") == -1
			RETURN
		ENDIF
		SELF:_SetThreadValue(Set.DateFormatNet, format:Replace("D","d"):Replace("Y","y"):Replace("/","'/'"))
		SELF:_SetThreadValue(Set.DateFormatEmpty, format:Replace("D"," "):Replace("Y"," "):Replace("M"," "))
		SELF:_SetThreadValue(Set.Century, format:Contains("YYYY"))
		SELF:_SetThreadValue(Set.DateFormat, format)
 		SWITCH format
		CASE "MM/DD/YY"
		CASE "MM/DD/YYYY" 
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.American)	 
		CASE "YY.MM.DD" 
		CASE "YYYY.MM.DD"
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.Ansi)	  
		CASE "DD/MM/YY"
		CASE "DD/MM/YYYY"
            // What a laugh, the British & french have an identical format. 
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.British)	
		CASE "DD.MM.YY"
		CASE "DD.MM.YYYY"
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.German)	
		CASE "DD-MM-YY"
		CASE "DD-MM-YYYY"
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.Italian)	
		CASE "YY/MM/DD"
		CASE "YYYY/MM/DD"
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.Japanese)	
		CASE "MM-DD-YY"
		CASE "MM-DD-YYYY"
			SELF:_SetThreadValue(Set.DateCountry, XSharp.DateCountry.USA)	
		END SWITCH

    INTERNAL METHOD _SetTimeFormatSystem() AS VOID
 		VAR dtInfo	    := System.Globalization.DateTimeFormatInfo.CurrentInfo
		SELF:_SetThreadValue(Set.AmExt, dtInfo:AMDesignator)
		SELF:_SetThreadValue(Set.PmExt, dtInfo:PMDesignator)
		VAR separator := dtInfo:TimeSeparator
		IF String.IsNullOrEmpty(separator)
			SELF:_SetThreadValue(Set.Timesep, (DWORD) 0)
		ELSE
			SELF:_SetThreadValue(Set.Timesep, (DWORD) separator[0])
		ENDIF
		SELF:_SetThreadValue(Set.AmPm, dtInfo:ShortDatePattern:IndexOf("tt") != -1)
        SELF:_SetThreadValue(Set.Timeformat, dtInfo:LongTimePattern)
       
    INTERNAL METHOD _SetDateFormatSystem() AS VOID
		LOCAL format    AS STRING
        VAR dtInfo	    := System.Globalization.DateTimeFormatInfo.CurrentInfo
        format  := dtInfo:ShortDatePattern:ToLower()
		// reduce to single m and d
		DO WHILE (format.IndexOf("mm") != -1)
        	format		:= format:Replace("mm", "m")
        ENDDO
	    DO WHILE format.IndexOf("dd") != -1
			format		:= format:Replace("dd", "d")
		ENDDO
		// make sure we have a double mm to get double digit dates
		// change dates to dd and mm and then everything to upper case
		format := format:Replace("d", "dd"):Replace("m","mm"):ToUpper()
        SELF:_SetDateFormat(format)
        RETURN


    INTERNAL METHOD _SetDateCountry(country AS XSharp.DateCountry) AS VOID
		
		LOCAL format, year AS STRING
        IF country == XSharp.DateCountry.System
            SELF:_SetDateFormatSystem()
        ELSE
		    year := IIF(SELF:_GetThreadValue<LOGIC>(Set.Century) , "YYYY" , "YY")
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
            SELF:_SetDateFormat(format) 
        ENDIF
        SELF:_SetThreadValue(Set.DateCountry, country)
        RETURN

	PRIVATE _Workareas AS Workareas
	/// <summary>The Workarea information for the current Thread.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
	PUBLIC STATIC PROPERTY Workareas AS Workareas
	GET
       LOCAL inst AS RuntimeState
        inst := GetInstance()
		IF inst:_Workareas == NULL_OBJECT
			inst:_Workareas := Workareas{}
		ENDIF
		RETURN inst:_Workareas
	END GET
    END PROPERTY
    /// <exclude />
    STATIC METHOD PushCurrentWorkarea(dwArea AS DWORD) AS VOID
        RuntimeState.Workareas:PushCurrentWorkarea(dwArea)
    /// <exclude />    
    STATIC METHOD PopCurrentWorkarea() AS DWORD
        RETURN RuntimeState.Workareas:PopCurrentWorkarea()

    [DebuggerBrowsable(DebuggerBrowsableState.Never)];
	PRIVATE _collationTable AS BYTE[]
    /// <summary>Current collation table.</summary>
    
	PUBLIC STATIC PROPERTY CollationTable AS BYTE[]
	GET
		LOCAL coll AS BYTE[]
		coll := GetInstance():_collationTable 
		IF coll == NULL .OR. coll :Length < 256
			_SetCollation("Generic")
			coll := GetInstance():_collationTable := GetValue<BYTE[]>(Set.CollationTable)
		ENDIF
		RETURN coll
	END GET
	SET
		GetInstance():_collationTable  := value
		SetValue(Set.CollationTable, value)
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
            _macrocompiler := value
        END SET
    END PROPERTY
        
    /// <summary>Active Macro compiler</summary>
    /// <remarks><note>This value is NOT 'per thread' but global for all threads.</note></remarks>
    PUBLIC STATIC PROPERTY MacroResolver AS MacroCompilerResolveAmbiguousMatch
        GET
            IF _macroresolver == NULL
                IF _macrocompiler IS IMacroCompiler2 VAR mc
                    _macroresolver := mc:Resolver 
                ENDIF
            endif
            RETURN _macroresolver
        END GET
        SET
            _macroresolver := value
            IF _macrocompiler IS IMacroCompiler2 VAR mc
                mc:Resolver := value
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
        IF _macrocompilerType == NULL_OBJECT
            VAR oMacroAsm := AssemblyHelper.Load("XSharp.MacroCompiler")
		    IF oMacroAsm != NULL_OBJECT
			    LOCAL oType AS System.Type
			    oType := oMacroAsm:GetType("XSharp.Runtime.MacroCompiler",FALSE,TRUE)
			    IF oType != NULL_OBJECT
				    // create instance of this type
				    IF TYPEOF(IMacroCompiler):IsAssignableFrom(oType)
					    _macrocompilerType := oType
				    ELSE
					    THROW Error{EG_CORRUPTION, "", "Could not create the macro compiler from the type "+ oType:FullName+" in the assembly "+oMacroAsm:Location}
				    ENDIF
			    ELSE
				    THROW Error{EG_CORRUPTION, "", "Could not load the macro compiler class in the assembly "+oMacroAsm:Location}
                ENDIF
            ELSE
                // AssemblyHelper.Load will throw an exception
                NOP 
		    ENDIF
        ENDIF
        IF _macrocompilerType != NULL_OBJECT
            _macrocompiler := Activator:CreateInstance(_macrocompilerType) ASTYPE IMacroCompiler
            IF _macrocompiler IS IMacroCompiler2 VAR mc
                mc:Resolver := _macroresolver
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
            IF IsRunningOnWindows()
                ret := XSharp.StringHelpers.CompareWindows(strLHS, strRHS)
            ELSE
                ret := String.Compare(strLHS, strRHS)
            ENDIF
        CASE CollationMode.Clipper  // both Clipper and XPP use weight tables
        CASE CollationMode.Xpp
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
        CASE CollationMode.Xpp
            RETURN XSharp.StringHelpers.CompareClipper(aLHS, aRHS, nLen)
        CASE CollationMode.Windows
            IF IsRunningOnWindows()
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
            NOP
        END SWITCH
        RETURN XSharp.StringHelpers.CompareOrdinal(aLHS, aRHS, nLen)
END CLASS




