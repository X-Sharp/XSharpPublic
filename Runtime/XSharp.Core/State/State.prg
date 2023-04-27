//
// Copyright (c) XSharp B.V.  All Rights Reserved.
// Licensed under the Apache License, Version 2.0.
// See License.txt in the project root for license information.
//

USING System.Collections.Generic
USING System.Threading
USING System.Diagnostics
USING System.Reflection
USING XSharp.RDD
USING XSharp.RDD.Enums



BEGIN NAMESPACE System.Runtime.CompilerServices
    INTERNAL STATIC CLASS IsExternalInit
    END CLASS
END NAMESPACE

    /// <summary>Delegate used for the StateChanged Event handler</summary>
DELEGATE XSharp.StateChanged (e AS StateChangedEventArgs) AS VOID
    /// <summary>Delegate used for the DialectChanged Event handler</summary>
DELEGATE XSharp.DialectChanged(oldDialect as XSharpDialect, newDialect as XSharpDialect) AS VOID

/// <summary>Arguments that are sent to StateChanged event handlers</summary>
CLASS XSharp.StateChangedEventArgs
    /// <summary>Setting that was just changed</summary>
    PROPERTY Setting  AS XSharp.Set AUTO GET INIT
    /// <summary>Old value of the setting</summary>
    PROPERTY OldValue AS OBJECT AUTO GET INIT
    /// <summary>New value of the setting</summary>
    PROPERTY NewValue AS OBJECT AUTO GET INIT
    INTERNAL CONSTRUCTOR()
        RETURN
END CLASS
/// <summary>
/// Container Class that holds the XSharp Runtime state
/// </summary>
/// <remarks>
/// Please note that unlike in most XBase products every thread has its own copy of the runtime state.<br/>
/// The runtime state from a new thread is a copy of the state of the main thread at that moment.
/// </remarks>
CLASS XSharp.RuntimeState

	// Static Fields
    PRIVATE INITONLY STATIC _onWindows AS LOGIC
	PRIVATE INITONLY STATIC _initialState  AS RuntimeState
	PRIVATE INITONLY _thread AS Thread
	PRIVATE STATIC _shutdown := FALSE AS LOGIC  // To prevent creating state when shutting down
	// Static Methods and Constructor
	PRIVATE STATIC currentState := ThreadLocal<RuntimeState>{ {=>  _initialState:Clone()},TRUE }  AS ThreadLocal<RuntimeState>
	STATIC CONSTRUCTOR
        AutoLock        := DoNothing
        AutoUnLock      := DoNothing
        detectDialect()
        SWITCH System.Environment.OSVersion:Platform
        CASE System.PlatformID.Win32NT
        CASE System.PlatformID.Win32S               // No longer in use
        CASE System.PlatformID.Win32Windows         // No longer in use
        CASE System.PlatformID.WinCE                // No longer in use
            _onWindows := TRUE
        OTHERWISE
            _onWindows := FALSE
        END SWITCH
        // This depends on the _onWindow flag so it must be last
        _initialState	:= RuntimeState{TRUE}



    PRIVATE STATIC METHOD detectDialect() AS VOID
        LOCAL asm := Assembly.GetEntryAssembly() AS Assembly
        VAR att := TYPEOF( XSharp.Internal.CompilerVersionAttribute )
        IF asm != NULL .AND. asm:IsDefined(att, FALSE)
            FOREACH VAR attr IN asm:GetCustomAttributes(att, FALSE)
                VAR compilerversion := (XSharp.Internal.CompilerVersionAttribute) attr
                VAR vers := compilerversion:Version
                var pos := vers:IndexOf("dialect:")
                if pos >= 0
                    vers := vers:Substring(pos + 8)
                    SWITCH vers:ToLower()
                    CASE "vo"
                        Dialect := XSharpDialect.VO
                    CASE "vulcan"
                        Dialect := XSharpDialect.Vulcan
                    CASE "core"
                        Dialect := XSharpDialect.Core
                    CASE "foxpro"
                        Dialect := XSharpDialect.FoxPro
                    CASE "harbour"
                        Dialect := XSharpDialect.Harbour
                    CASE "xpp"
                        Dialect := XSharpDialect.XPP
                    END SWITCH
                endif
            NEXT
        ENDIF

    PUBLIC STATIC PROPERTY RunningOnWindows as LOGIC GET _onWindows

	/// <summary>Retrieve the runtime state for the current thread</summary>
	PUBLIC STATIC METHOD GetInstance() AS RuntimeState
		RETURN currentState:Value
    /// <summary>This event is invoked when the runtime state is changed.</summary>
    PUBLIC STATIC EVENT StateChanged AS StateChanged
    /// <summary>This event is invoked when the runtime dialect is changed.</summary>
    /// <remarks>Normally this will almost never happen. However inside our test suites this is quite common </remarks>
    PUBLIC STATIC EVENT DialectChanged as DialectChanged
    [DebuggerBrowsable(DebuggerBrowsableState.Never)];
	PRIVATE oSettings AS Dictionary<XSharp.Set, OBJECT>
    /// <summary>The dictionary that stores most of the settings in the runtime state. The key to the index is the number from the Set Enum</summary>
    /// <seealso cref="Set" >Set Enum</seealso>
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
            // Some values that are not in the dictionary
            RuntimeState.FileException := NULL
            RuntimeState.FileError     := 0
            RuntimeState.LastRddError  := NULL

            SELF:_SetThreadValue<Exception>(Set.Patharray,NULL)
            SELF:_SetThreadValue<BYTE[]>(Set.CollationTable, NULL )
			IF RuntimeState.RunningOnWindows
                SELF:_SetThreadValue(Set.DosCodepage, Win32.GetDosCodePage())
                SELF:_SetThreadValue(Set.WinCodepage, Win32.GetWinCodePage())
            ELSE
                SELF:_SetThreadValue(Set.DosCodepage, 437L)  // US American
                SELF:_SetThreadValue(Set.WinCodepage, 1252L) // Latin 1 / Western Europe
                SELF:_SetThreadValue(Set.CollationMode, CollationMode.Unicode )
            ENDIF
			// Date and time settings
			SELF:_SetInternationalWindows()


		ENDIF
		RETURN
    /// <exclude />
	DESTRUCTOR()
		// What do we need to clean ?
        IF SELF == _initialState
            _shutdown := TRUE
        ENDIF


	PRIVATE METHOD Clone() AS RuntimeState
		LOCAL oNew AS RuntimeState
        IF Thread.CurrentThread == _initialState:_thread .OR. _shutdown
            RETURN _initialState
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
            LOCAL oResult AS object
			IF oSettings:TryGetValue(nSetting, OUT oResult)
				result := (T) oResult
			ELSE
				result := DEFAULT(T)
			ENDIF
            IF StateChanged != NULL
                VAR args := StateChangedEventArgs{} {Setting := nSetting, OldValue := oResult, NewValue := oValue}
                StateChanged( args )
            ENDIF
			oSettings[nSetting] := oValue
		END LOCK
		RETURN	result

	#region properties FROM the Vulcan RuntimeState that are emulated

	/// <summary>The current compiler setting for the VO13 compiler option.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default value for this option is 'False'.</value>
	STATIC PROPERTY CompilerOptionFox2 AS LOGIC AUTO

	/// <summary>The current compiler setting for the VO11 compiler option.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default vale for this option is 'False'.</value>
	STATIC PROPERTY CompilerOptionVO11 AS LOGIC AUTO

	/// <summary>The current compiler setting for the VO13 compiler option.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default value for this option is 'False'.</value>
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
    /// <remarks>When the dialect changes then registered DialectChanged Event Handlers will be called </remarks>
    /// <seealso cref="DialectChanged" />
    STATIC PRIVATE _dialect := XSharpDialect.Core AS XSharpDialect
	STATIC PROPERTY Dialect AS XSharpDialect
        GET
            RETURN _dialect
        END GET
        SET
            var old := _dialect
            _dialect := value
            IF DialectChanged != NULL .and. old != value
                DialectChanged(old, value)
            ENDIF
        END SET
    END PROPERTY

	/// <summary>The current compiler setting for the FOVF compiler option.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    /// <value>The default vale for this option is 'False'.</value>
	STATIC PROPERTY CompilerOptionFOVF AS LOGIC AUTO

	/// <summary>The System.Reflection.Module for the main application.</summary>
    /// <include file="CoreComments.xml" path="Comments/CompilerOptions/*" />
    STATIC PROPERTY AppModule AS  Module AUTO
	#endregion

	/// <summary>The last file found with File(). This is the name that FPathName() returns.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="File" />
    /// <seealso cref="FPathName" />
    /// <seealso cref="Set.LastFound" />
    STATIC PROPERTY LastFound AS STRING ;
        GET GetValue<STRING>(Set.LastFound);
        SET SetValue<STRING>(Set.LastFound, value)

	/// <summary>The last File IO error number</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="FError" />
    STATIC PROPERTY FileError AS DWORD AUTO

	/// <summary>The last file IO Exception</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="FException" />
    STATIC PROPERTY FileException AS Exception AUTO

	/// <summary>The current Alternate setting.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="O:XSharp.RT.Functions.SetAlternate" />
    /// <seealso cref="O:XSharp.RT.Functions.SetAltFile" />
    /// <seealso cref="Set.Alternate" />
   static property Alternate as logic ;
        GET GetValue<LOGIC>(Set.Alternate);
        SET SetValue<LOGIC>(Set.Alternate, value)

    /// <summary>The current AltFile setting.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="O:XSharp.RT.Functions.SetAlternate" />
    /// <seealso cref="O:XSharp.RT.Functions.SetAltFile" />
    /// <seealso cref="Set.AltFile" />
   STATIC PROPERTY AltFile AS STRING ;
        GET GetValue<STRING>(Set.AltFile);
        SET SetValue<STRING>(Set.AltFile, value)


	/// <summary>The current ANSI setting</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetAnsi" />
    /// <seealso cref="Set.Ansi" />
    STATIC PROPERTY Ansi AS LOGIC ;
        GET GetValue<LOGIC>(Set.Ansi);
        SET SetValue<LOGIC>(Set.Ansi, value)

	/// <summary>The current AutoOrder setting (used by the RDD system).</summary>
    /// <seealso cref="AutoOpen" />
    /// <seealso cref="AutoShareMode" />
    /// <seealso cref="Set.AutoOrder" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY AutoOrder AS LONG ;
        GET GetValue<LONG>(Set.AutoOrder);
        SET SetValue<LONG>(Set.AutoOrder, value)

	/// <summary>The current AutoOpen setting (used by the RDD system).</summary>
    /// <seealso cref="AutoOrder" />
    /// <seealso cref="AutoShareMode" />
    /// <seealso cref="Set.AutoOpen" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY AutoOpen AS LOGIC ;
        GET GetValue<LOGIC>(Set.AutoOpen);
        SET SetValue<LOGIC>(Set.AutoOpen, value)

	/// <summary>The current AutoShareMode setting (used by the RDD system).</summary>
    /// <seealso cref="AutoOpen" />
    /// <seealso cref="AutoOrder" />
    /// <seealso cref="Set.Autoshare" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY AutoShareMode AS LONG ;
        GET (LONG) GetValue<AutoShareMode>(Set.Autoshare);
        SET SetValue<AutoShareMode>(Set.Autoshare, (AutoShareMode)value)


	/// <summary>The current Compatible setting.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetCompatible" />
    /// <seealso cref="Set.Compatible" />
   STATIC PROPERTY Compatible AS LOGIC ;
        GET GetValue<LOGIC>(Set.Compatible);
        SET SetValue<LOGIC>(Set.Compatible, VALUE)

	/// <summary>The current Console setting.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="O:XSharp.RT.Functions.SetConsole" />
    /// <seealso cref="Set.Console" />
   STATIC PROPERTY Console AS LOGIC ;
        GET GetValue<LOGIC>(Set.Console);
        SET SetValue<LOGIC>(Set.Console, value)

	/// <summary>The current Century setting (used in DATE &lt;-&gt; STRING conversions).</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetCentury" />
    /// <seealso cref="Set.Century" />
   STATIC PROPERTY Century AS LOGIC ;
        GET GetValue<LOGIC>(Set.Century);
        SET SetValue<LOGIC>(Set.Century, value)

	/// <summary>The current Collation mode (used by the RDD system).</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="Set.CollationMode" />
    /// <seealso cref="OnCollationChanged" />
    /// <remarks>Send an OnCollationChanged event when changed.</remarks>
   static property CollationMode as CollationMode
        get
			return GetValue<CollationMode>(Set.CollationMode)
		END GET
        SET
			SetValue<CollationMode>(Set.CollationMode, value)
			if OnCollationChanged != null
				OnCollationChanged(GetInstance(), EventArgs{})
			ENDIF
		END SET
	END PROPERTY

	/// <summary>The current DateCountry setting mode (used in DATE &lt;-&gt; STRING conversions).</summary>
    /// <seealso cref="DateFormat" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetDateCountry" />
    /// <seealso cref="Set.DateCountry" />
   STATIC PROPERTY DateCountry AS DWORD ;
        GET (DWORD) GetValue<XSharp.DateCountry>(Set.DateCountry);
        SET RuntimeState.GetInstance():_SetDateCountry( (XSharp.DateCountry) value)

	/// <summary>The current Date format</summary>
	/// <remarks>This string should contain a combination of DD MM and either YY or YYYY characters.<br/>
	/// For example DD-MM-YYYY for italian date format, MM/DD/YYYY for American date format or DD/MM/YYYY for British Date format.
	/// Note that all other characters except the four groups mentioned above are copied to the output string verbatim.
    /// <note>This value is 'per thread' </note></remarks>
    /// <seealso cref="DateCountry" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="GetDateFormat" />
    /// <seealso cref="SetDateFormat" />
    /// <seealso cref="Set.DateFormat" />
    STATIC PROPERTY DateFormat AS STRING ;
        GET GetValue<STRING>(Set.DateFormat);
        SET RuntimeState.GetInstance():_SetDateFormat(value)

    /// <summary>A cached copy of the string that is returned for empty dates, matching the current DateFormat</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="Set.DateFormatEmpty" />
	STATIC PROPERTY NullDateString AS STRING GET GetValue<STRING>(Set.DateFormatEmpty)

	/// <summary>The default number of decimals for new FLOAT values that are created without explicit decimals.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetDecimal" />
    /// <seealso cref="Set.Decimals" />
    STATIC PROPERTY Decimals AS DWORD ;
        GET GetValue<DWORD>(Set.Decimals);
        SET SetValue<DWORD>(Set.Decimals, value)


	/// <summary>The default number of decimals for new FLOAT values that are created without explicit decimals.</summary>
    /// <seealso cref="ThousandSep" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetDecimalSep" />
    /// <seealso cref="Set.DecimalSep" />
    STATIC PROPERTY DecimalSep AS DWORD ;
        GET GetValue<DWORD>(Set.DecimalSep);
        SET SetValue<DWORD>(Set.DecimalSep, value)

	/// <summary>The default RDD.</summary>
    /// <remarks><note>This value is 'per thread' </note></remarks>
    /// <seealso cref="O:XSharp.RT.Functions.RddSetDefault" />
    STATIC PROPERTY DefaultRDD AS STRING ;
        GET GetValue<STRING>(Set.DefaultRdd);
        SET SetValue<STRING>(Set.DefaultRdd, value)


	/// <summary>RDD Deleted Flag that determines whether to ignore or include records that are marked for deletion.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetDeleted" />
    /// <seealso cref="Set.Deleted" />
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
    /// <seealso cref="SetDigit" />
    /// <seealso cref="Set.Digits" />
    STATIC PROPERTY Digits AS DWORD ;
        GET GetValue<DWORD>(Set.Digits);
        SET SetValue<DWORD>(Set.Digits, value)

    /// <summary>Logical setting that fixes the number of digits used to display numeric output.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetDigitFixed" />
    /// <seealso cref="Set.DigitFixed" />
    STATIC PROPERTY DigitsFixed AS LOGIC ;
        GET GetValue<LOGIC>(Set.DigitFixed);
        SET SetValue<LOGIC>(Set.DigitFixed, value)


	/// <summary>The 'DOS' Codepage. This gets read at startup from the OS().</summary>
    /// <seealso cref="DosEncoding" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="OnCodePageChanged" />
    /// <remarks>Sends an OnCodePageChanged event when changed.</remarks>
    STATIC PROPERTY DosCodePage AS LONG
        GET
            RETURN GetValue<LONG>(Set.DosCodepage)
		END GET
        SET
			SetValue<LONG>(Set.DosCodepage, VALUE)
			IF OnCodePageChanged != NULL
				OnCodePageChanged(GetInstance(), EventArgs{})
			ENDIF
		END SET
	END PROPERTY

	/// <summary>Should text files, such as memo files be written with a closing EOF = chr(26) character.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="Set.Eof" />
    STATIC PROPERTY Eof AS LOGIC ;
        GET GetValue<LOGIC>(Set.Eof);
        SET SetValue<LOGIC>(Set.Eof, value)


	/// <summary>End of line character to be used by the runtime. Defaults to CR + LF (13 + 13).</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="Set.Eol" />
    STATIC PROPERTY Eol AS STRING ;
        GET GetValue<STRING>(Set.Eol);
        SET SetValue<STRING>(Set.Eol, value)


	/// <summary>Date Epoch value that determines how dates without century digits are interpreted.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetEpoch" />
    /// <seealso cref="Set.Epoch" />

    STATIC PROPERTY Epoch AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.Epoch));
        SET SetValue<DWORD>(Set.Epoch, value)

	/// <summary>Date Epoch Year value. This gets set by the SetEpoch() function to the Epoch year % 100.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetEpoch" />
    /// <seealso cref="Set.EpochYear" />
    STATIC PROPERTY EpochYear AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.EpochYear));

	/// <summary>Date Epoch Century value. This gets set by the SetEpoch() function to the century in which the Epoch year falls.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetEpoch" />
    /// <seealso cref="Set.EpochCent" />
    STATIC PROPERTY EpochCent AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.EpochCent));


	/// <summary>String comparison Exact flag that determines how comparisons with the single '=' characters should be done.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetExact" />
    /// <seealso cref="Set.Exact" />
    static property Exact as logic ;
        get GetValue<logic>(Set.Exact);
        set SetValue<logic>(Set.Exact, value)

    /// <summary>Logical setting that fixes the number of decimal digits used to display numbers.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetFixed" />
    /// <seealso cref="Set.Fixed" />
    STATIC PROPERTY Fixed AS LOGIC ;
        GET GetValue<LOGIC>(Set.Fixed);
        SET SetValue<LOGIC>(Set.Fixed, value)



	/// <summary>Numeric value that controls the precision of Float comparisons.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="O:XSharp.RT.Functions.SetFloatDelta" />
    /// <seealso cref="Set.Floatdelta" />
   STATIC PROPERTY FloatDelta AS REAL8 ;
        GET GetValue<REAL8>(Set.Floatdelta);
        SET SetValue<REAL8>(Set.Floatdelta, value)

	/// <summary>Current SetInternational Setting.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetInternational" />
    /// <seealso cref="Set.Intl" />
     STATIC PROPERTY International AS CollationMode ;
        GET GetValue<CollationMode>(Set.Intl);
        SET SetValue<CollationMode>(Set.Intl, value)

	/// <summary>Last error that occurred in the RDD subsystem.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY LastRddError AS Exception AUTO

	/// <summary>Last Script error that occurred .</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="Set.LastScriptError" />
    STATIC PROPERTY LastScriptError AS Exception ;
        GET GetValue<Exception>(Set.LastScriptError);
        SET SetValue<Exception>(Set.LastScriptError, value)

	/// <summary>Number of tries that were done when the last lock operation failed.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetEpoch" />
    /// <seealso cref="Set.EpochCent" />
    STATIC PROPERTY LockTries AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.Locktries));
        SET SetValue<DWORD>(Set.Locktries, value)

    /// <summary>The setting that determines whether to use the High Performance (HP) locking schema for newly created .NTX files</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="Set.HpLocking" />
    STATIC PROPERTY HPLocking AS LOGIC ;
        GET GetValue<LOGIC>(Set.HpLocking);
        SET SetValue<LOGIC>(Set.HpLocking, value)

    /// <summary>The setting that determines whether to use the new locking offset of -1 (0xFFFFFFFF) for .NTX files.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="Set.NewIndexLock" />
    STATIC PROPERTY NewIndexLock AS LOGIC ;
        GET GetValue<LOGIC>(Set.NewIndexLock);
        SET SetValue<LOGIC>(Set.NewIndexLock, value)


	/// <summary>The current default MemoBlock size.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="Set.MemoBlockSize" />
    STATIC PROPERTY MemoBlockSize AS WORD;
        GET Convert.ToUInt16(GetValue<OBJECT>(Set.MemoBlockSize));
        SET SetValue<WORD>(Set.MemoBlockSize, value)


	/// <summary>Did the last RDD operation cause a Network Error ?</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="Set.Neterr" />
    STATIC PROPERTY NetErr AS LOGIC;
        GET GetValue<LOGIC>(Set.Neterr);
        SET SetValue<LOGIC>(Set.Neterr, value)

	/// <summary>RDD Optimize Flag  </summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="Set.Optimize" />
    STATIC PROPERTY Optimize AS LOGIC ;
        GET GetValue<LOGIC>(Set.Optimize);
        SET SetValue<LOGIC>(Set.Optimize, value)


    /// <summary>The current SetRefresh flag.</summary>
   /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
   /// <seealso cref="SetRefresh" />
   /// <seealso cref="Set.Refresh" />
   STATIC PROPERTY Refresh AS REAL8 ;
       GET GetValue<REAL8>(Set.Refresh);
       SET SetValue<REAL8>(Set.Refresh, value)

	/// <summary>The current SetSafety flag.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetSafety" />
    /// <seealso cref="Set.Safety" />
    STATIC PROPERTY Safety AS LOGIC ;
        GET GetValue<LOGIC>(Set.Safety);
        SET SetValue<LOGIC>(Set.Safety, value)

	/// <summary>The current SetSoftSeek flag.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetSoftSeek" />
    /// <seealso cref="Set.Softseek" />
    STATIC PROPERTY SoftSeek AS LOGIC ;
        GET GetValue<LOGIC>(Set.Softseek);
        SET SetValue<LOGIC>(Set.Softseek, value)

	/// <summary>The Thousand separator</summary>
    /// <seealso cref="DecimalSep" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetThousandSep" />
    /// <seealso cref="Set.ThousandSep" />
    STATIC PROPERTY ThousandSep AS DWORD ;
        GET Convert.ToUInt32(GetValue<OBJECT>(Set.ThousandSep));
        SET SetValue<DWORD>(Set.ThousandSep, value)


	/// <summary>Should indexes by default be created with the 'Unique setting'.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="SetUnique" />
    /// <seealso cref="Set.Unique" />
    STATIC PROPERTY Unique AS LOGIC ;
        GET GetValue<LOGIC>(Set.Unique);
        SET SetValue<LOGIC>(Set.Unique, value)

	/// <summary>The Windows Codepage. This gets read at startup from the OS().</summary>
    /// <seealso cref="WinEncoding" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="Set.WinCodepage" />
    /// <seealso cref="OnCodePageChanged" />
    /// <remarks>Sends an OnCodePageChanged event when changed.</remarks>
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

    /// <summary>The Windows Encoding. This is based on the corrent Win Codepage.</summary>
    /// <seealso cref="WinCodePage" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="O:RuntimeState.StringCompare" />
    STATIC PROPERTY WinEncoding AS System.Text.Encoding ;
        GET System.Text.Encoding.GetEncoding(WinCodePage)

    /// <summary>The DOS Encoding. This is based on the corrent DOS Codepage.</summary>
    /// <seealso cref="DosCodePage" />
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    STATIC PROPERTY DosEncoding AS System.Text.Encoding ;
        GET System.Text.Encoding.GetEncoding(DosCodePage)


	/// <summary>The name of the method that was called in the last late bound method call.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
    /// <seealso cref="Set.NoMethod" />
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

	PRIVATE _dataSession AS DataSession
	/// <summary>The Workarea information for the current Thread.</summary>
    /// <remarks>This property is a backward compatible alias for the DataSession property</remarks>
    /// <seealso cref="DataSession" />
    PUBLIC STATIC PROPERTY Workareas AS DataSession GET DataSession
	/// <summary>The DataSession information for the current Thread.</summary>
    /// <include file="CoreComments.xml" path="Comments/PerThread/*" />
	PUBLIC STATIC PROPERTY DataSession AS DataSession
	GET
        LOCAL inst AS RuntimeState
        inst := GetInstance()
        IF inst:_dataSession == NULL_OBJECT
            LOCAL name AS STRING
            VAR thread := System.Threading.Thread.CurrentThread
            IF String.IsNullOrEmpty(thread:Name)
                name := thread:ManagedThreadId:ToString()
            ELSE
                name := thread:Name
            ENDIF
            IF inst == _initialState
                inst:_dataSession := DataSession{1, "Global datasession"}
            ELSE
                inst:_dataSession := DataSession{"DataSession for Thread "+name}
            ENDIF
        ENDIF
        RETURN inst:_dataSession
    END GET
    END PROPERTY

    /// <summary>This method can be used to switch the active DataSession in the runtime state.</summary>
    /// <returns>The previous active datasession</returns>
    /// <param name="session">The datasession that needs to be set as the new active datasession</param>
    /// <remarks>This sets the DataSession in the <em>current thread</em>.</remarks>
    STATIC METHOD SetDataSession (session as DataSession) AS DataSession
        LOCAL inst AS RuntimeState
        LOCAL old  as DataSession
        inst := GetInstance()
        old := inst:_dataSession
        inst:_dataSession := session
        RETURN old

    /// <exclude />
    STATIC METHOD PushCurrentWorkarea(dwArea AS DWORD) AS VOID
        RuntimeState.Workareas:PushCurrentWorkarea(dwArea)
    /// <exclude />
    STATIC METHOD PopCurrentWorkarea() AS DWORD
        RETURN RuntimeState.Workareas:PopCurrentWorkarea()

    [DebuggerBrowsable(DebuggerBrowsableState.Never)];
	PRIVATE _collationTable AS BYTE[]

    /// <summary>Delegate that gets called to automatically lock a record in the FoxPro dialect.</summary>
    static AutoLock   as AutoLockMethod
    /// <summary>Delegate that gets called to automatically unlock a record in the FoxPro dialect.</summary>
    static AutoUnLock as AutoLockMethod
    /// <summary>Delegate that describes the signature of the AutoLock methods </summary>
    delegate AutoLockMethod() as void

    /// <exclude />
    STATIC METHOD DoNothing() AS VOID
        return

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
		if OnCollationChanged != null
			OnCollationChanged(GetInstance(), EventArgs{})
		ENDIF
	END SET
	END PROPERTY

    /// <summary>Active Script compiler. Gets assigned from the ExecScriptFast() function.</summary>
    /// <remarks><note>This value is NOT 'per thread' but global for all threads.</note></remarks>
    STATIC PUBLIC PROPERTY ScriptCompiler AS IMacroCompiler AUTO := NULL
	STATIC INTERNAL _macrocompilerType   AS System.Type
    STATIC INTERNAL _macrocompiler       AS IMacroCompiler
    STATIC INTERNAL _macroresolver       AS MacroCompilerResolveAmbiguousMatch
    /// <summary>Active Macro compiler</summary>
    /// <remarks><note>This value is NOT 'per thread' but global for all threads.</note></remarks>
    /// <seealso cref="IMacroCompiler" />
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

    /// <summary>
    /// This property allows you to override the mechanism that decided if the types in an assembly should be cached
    /// The default behavior is that every assembly that is found is cached.
    /// </summary>
    /// <value></value>
    PUBLIC STATIC PROPERTY MacroCompilerIncludeAssemblyInCache as MacroCompilerIncludeAssemblyInCache AUTO := { a AS Assembly => TRUE}


    /// <summary>Active Macro compiler</summary>
    /// <remarks><note>This value is NOT 'per thread' but global for all threads.</note></remarks>
    /// <seealso cref="IMacroCompiler2" />
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
	/// <summary>This event is thrown when one of the codepages of the runtimestate is changed</summary>
    /// <remarks>Clients can refresh cached information by registering to this event</remarks>
    /// <seealso cref="DosCodePage" />
    /// <seealso cref="WinCodePage" />
	PUBLIC STATIC EVENT OnCodePageChanged AS EventHandler
	/// <summary>This event is thrown when the collation of the runtimestate is changed</summary>
    /// <remarks>Clients can refresh cached information by registering to this event</remarks>
    /// <seealso cref="CollationMode" />
	public static event OnCollationChanged as EventHandler

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
            _macrocompiler := Activator.CreateInstance(_macrocompilerType) ASTYPE IMacroCompiler
            IF _macrocompiler IS IMacroCompiler2 VAR mc
                mc:Resolver := _macroresolver
            endif
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
    /// <seealso cref="P:CompilerOptionVO13" />
    /// <seealso cref="P:Exact" />
    /// <seealso cref="M:StringCompareCollation" />
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
    /// <seealso cref="O:StringCompare" />
    /// <seealso cref="P:CollationMode" />
    static method StringCompareCollation(strLHS as string, strRHS as string) as int
       LOCAL ret AS INT
        // either exact or RHS longer than LHS
        VAR mode := RuntimeState.CollationMode
        SWITCH mode
        CASE CollationMode.Windows
            IF RuntimeState.RunningOnWindows
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

    /// <inheritdoc cref="XSharp.RuntimeState.StringCompare"/>
    /// <summary>Compare 2 byte arrays respecting the runtime string comparison rules.</summary>
    /// <param name="aLHS">The first list of bytes.</param>
    /// <param name="aRHS">The second list of bytes.</param>
    /// <param name="nLen">The # of bytes to compare.</param>
    /// <remarks>This method works on BYTE arrays and is used by the RDD system. <br/>
    /// This method respects the current setting of SetCollation(): <br/>
    /// - When the current collationmode is Clipper or Windows then no Ansi - Unicode conversions will be done.The comparisons will be done on the byte arrays.<br/>
    /// - When the current collationmode is Unicode or Ordinal then the byte arrays will be converted to Unicode before the comparison is executed.
    /// </remarks>
    /// <seealso cref="WinEncoding" />
    /// <seealso cref="StringCompareCollation" />
    /// <seealso cref="CollationMode" />
    /// <seealso cref="SetCollation" />
    STATIC METHOD StringCompare(aLHS AS BYTE[], aRHS AS BYTE[], nLen AS INT) AS INT
        SWITCH CollationMode
        CASE CollationMode.Clipper
        CASE CollationMode.Xpp
            RETURN XSharp.StringHelpers.CompareClipper(aLHS, aRHS, nLen)
        CASE CollationMode.Windows
            IF RuntimeState.RunningOnWindows
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
